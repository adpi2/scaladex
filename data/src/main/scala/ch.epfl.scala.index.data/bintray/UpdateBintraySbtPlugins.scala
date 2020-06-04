package ch.epfl.scala.index.data.bintray

import java.nio.file.{Files, Path}
import java.time.format.DateTimeFormatter
import java.time.{OffsetDateTime, ZoneOffset}

import akka.actor.ActorSystem
import akka.stream.Materializer
import ch.epfl.scala.index.data.DataPaths
import ch.epfl.scala.index.data.cleanup.GithubRepoExtractor
import ch.epfl.scala.index.data.github.GithubDownload
import com.typesafe.scalalogging.LazyLogging
import org.apache.ivy.core.settings.IvySettings
import org.apache.ivy.plugins.parser.xml.XmlModuleDescriptorParser

import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.matching.Regex

class UpdateBintraySbtPlugins(
   bintray: BintrayClient,
   sbtPluginRepo: SbtPluginsData,
   githubRepo: GithubRepoExtractor,
   githubDownload: GithubDownload,
   lastDownloadPath: Path
)(implicit val ec: ExecutionContext)
    extends LazyLogging {
  private val dateTimeFormatter = DateTimeFormatter.ISO_DATE_TIME
  private val moduleDescriptorParser = XmlModuleDescriptorParser.getInstance()

  private val baseSubject = "sbt"
  private val baseRepo = "sbt-plugin-releases"

  /** Matches an ivy.xml file of an sbt-plugin release
   * <organization>/<artifact>/scala_<version>/sbt_<version>/<version>/ivys/ivy.xml
   */
  private val sbtPluginPathRegex: Regex =
    "(.+?)/(.+?)/scala_(.+?)/sbt_(.+?)/(.+?)/ivys/ivy.xml".r

  def update(): Future[Unit] = {
    val lastUpdate = parse(Files.readAllLines(lastDownloadPath).get(0))

    val oldReleasesF = Future(sbtPluginRepo.read())
    val newReleasesF = getNewReleases(lastUpdate)

    for {
      oldReleases <- oldReleasesF
      newReleases <- newReleasesF
    } yield {
      updateGithub(newReleases)
      sbtPluginRepo.update(oldReleases, newReleases)
      val now = format(OffsetDateTime.now(ZoneOffset.UTC))
      Files.write(lastDownloadPath, Seq(now).asJava)
    }
  }

  private def getNewReleases(
      lastUpdate: OffsetDateTime
  ): Future[Seq[SbtPluginReleaseModel]] = {
    for {
      packageNames <- bintray.getAllPackages(baseSubject, baseRepo)
      allPackages <- Future.traverse(packageNames)(
        bintray.getPackage(baseSubject, baseRepo, _)
      )
      packagesToUpdate = allPackages.filter(
        p => parse(p.updated).isAfter(lastUpdate)
      )
      packagesByRepo = packagesToUpdate.groupBy(p => (p.owner, p.repo))

      _ = logger.info(s"found ${packagesToUpdate.size} packages to update in ${packagesByRepo.keys.size} repositories")

      sbtPlugins <- Future
        .traverse(packagesByRepo) {
          case ((owner, repo), packages) =>
            getSbtPlugins(owner, repo, packages, lastUpdate)
        }
        .map(_.flatten.toSeq)

    } yield sbtPlugins
  }

  private def getSbtPlugins(
      owner: String,
      repo: String,
      packages: Seq[BintrayPackage],
      lastUpdate: OffsetDateTime
  ): Future[Seq[SbtPluginReleaseModel]] = {
    logger.info(s"updating ${packages.size} packages from $owner/$repo")
    val packageByName = packages.map(p => p.name -> p).toMap
    val createdAfter = format(lastUpdate)
    for {
      allIvyFiles <- bintray.searchFiles(owner, repo, "ivy.xml", createdAfter)
      filteredIvyFiles = allIvyFiles
        .filter(file => packageByName.contains(file.`package`))
      sbtPlugins <- Future
        .traverse(filteredIvyFiles)(
          file => downloadPluginDescriptor(owner, repo, packageByName(file.`package`), file)
        )
        .map(_.flatten)
    } yield sbtPlugins
  }

  private def downloadPluginDescriptor(
      owner: String,
      repo: String,
      `package`: BintrayPackage,
      file: BintraySearch
  ): Future[Option[SbtPluginReleaseModel]] = {
    file.path match {
      case sbtPluginPathRegex(org,
                              artifact,
                              scalaVersion,
                              sbtVersion,
                              version) =>
        Future {
          try {
            val descriptor = moduleDescriptorParser.parseDescriptor(
              new IvySettings(),
              bintray.downloadUrl(owner, repo, file.path),
              /* validate =*/ false
            )

            Some {
              SbtPluginReleaseModel(
                owner,
                repo,
                Option(`package`.vcs_url),
                org,
                artifact,
                scalaVersion,
                sbtVersion,
                version,
                file.path,
                file.sha1,
                descriptor
              )
            }
          } catch {
            case NonFatal(cause) =>
              logger.warn(s"Unable to fetch ${file.path}: ${cause.getMessage}")
              None
          }
        }

      case _ => Future.successful(None)
    }
  }

  private def updateGithub(sbtPlugins: Seq[SbtPluginReleaseModel]): Unit = {
    val githubRepos = sbtPlugins.flatMap(plugin => githubRepo(plugin.releaseModel)).toSet
    logger.info(s"updating ${githubRepos.size} Github repositories")
    githubDownload.run(githubRepos)
  }

  private def parse(dateTime: String): OffsetDateTime =
    OffsetDateTime.from(dateTimeFormatter.parse(dateTime))
  private def format(dateTime: OffsetDateTime): String =
    dateTimeFormatter.format(dateTime)

}

object UpdateBintraySbtPlugins {
  def run(paths: DataPaths)(implicit mat: Materializer,
                            sys: ActorSystem): Unit = {
    implicit val ec: ExecutionContext = sys.dispatcher
    val githubDownload = new GithubDownload(paths)
    val githubRepoExtractor = new GithubRepoExtractor(paths)
    for (bintrayClient <- BintrayClient.create(paths.credentials)) {
      val sbtPluginsData = SbtPluginsData(paths.ivysData)
      val updater = new UpdateBintraySbtPlugins(
        bintrayClient,
        sbtPluginsData,
        githubRepoExtractor,
        githubDownload,
        paths.ivysLastDownload
      )
      Await.result(updater.update(), Duration.Inf)
    }
  }
}
