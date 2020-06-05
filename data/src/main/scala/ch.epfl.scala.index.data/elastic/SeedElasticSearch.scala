package ch.epfl.scala.index
package data
package elastic

import ch.epfl.scala.index.data.github.GithubDownload
import ch.epfl.scala.index.data.maven.PomsReader
import ch.epfl.scala.index.data.project._
import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s.analyzers._
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Success

class SeedElasticSearch(paths: DataPaths, githubDownload: GithubDownload)(
    implicit val ec: ExecutionContext
) extends ProjectProtocol {

  private val log = LoggerFactory.getLogger(getClass)

  def run(): Unit = {

    val exists = Await
      .result(
        esClient.execute(indexExists(indexName)),
        Duration.Inf
      )
      .isExists

    if (exists) {
      Await.result(
        esClient.execute(deleteIndex(indexName)),
        Duration.Inf
      )
    }

    val projectFields = List(
      textField("organization")
        .analyzer("standard")
        .fields(
          keywordField("keyword").normalizer("lowercase")
        ),
      textField("repository")
        .analyzer("standard")
        .fields(
          keywordField("keyword").normalizer("lowercase")
        ),
      textField("primaryTopic")
        .analyzer("english")
        .fields(
          keywordField("keyword").normalizer("lowercase")
        ),
      keywordField("defaultArtifact").index(false),
      keywordField("artifacts").normalizer("lowercase"),
      keywordField("customScalaDoc").index(false),
      keywordField("artifactDeprecations").index(false),
      keywordField("cliArtifacts").index(false),
      keywordField("targets"),
      keywordField("dependencies"),
      objectField("github").fields(
        textField("topics")
          .analyzer("standard")
          .fields(
            keywordField("keyword").normalizer("lowercase")
          ),
        nestedField("beginnerIssues"),
        textField("description").analyzer("english"),
        textField("readme").analyzer("english_readme")
      ),
      dateField("created"),
      dateField("updated")
    )

    val releasesFields = List(
      nestedField("reference")
        .fields(
          keywordField("organization") normalizer "lowercase",
          keywordField("repository") normalizer "lowercase",
          keywordField("artifact") normalizer "lowercase"
        )
        .includeInAll(true),
      nestedField("maven").fields(
        keywordField("groupId") normalizer "lowercase",
        keywordField("artifactId") normalizer "lowercase",
        keywordField("version")
      ),
      keywordField("version"),
      keywordField("targetType") normalizer "lowercase",
      keywordField("scalaVersion"),
      keywordField("scalaJsVersion"),
      keywordField("scalaNativeVersion"),
      keywordField("sbtVersion"),
      dateField("released")
    )

    log.info("creating index")

    val urlStrip = PatternReplaceCharFilter(
      "url_strip",
      "https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()@:%_\\+.~#?&//=]*)",
      ""
    )
    val codeStrip = PatternReplaceCharFilter(
      "code_strip",
      "<code>[\\w\\W]*?<\\/code>",
      ""
    )
    val englishStop = StopTokenFilter(
      "english_stop",
      language = Some(NamedStopTokenFilter.English)
    )
    val englishStemmer = StemmerTokenFilter("english_stemmer", "english")
    val englishPossessiveStemmer = StemmerTokenFilter(
      "english_possessive_stemmer",
      "possessive_english"
    )

    val englishReadme =
      CustomAnalyzerDefinition(
        "english_readme",
        StandardTokenizer,
        codeStrip,
        HtmlStripCharFilter,
        urlStrip,
        LowercaseTokenFilter,
        englishPossessiveStemmer,
        englishStop,
        englishStemmer
      )

    val lowercase = customNormalizer("lowercase", LowercaseTokenFilter)

    Await.result(
      esClient.execute {
        createIndex(indexName)
          .analysis(englishReadme)
          .normalizers(lowercase)
          .mappings(
            mapping(projectsCollection).fields(projectFields: _*),
            mapping(releasesCollection).fields(releasesFields: _*)
          )
      },
      Duration.Inf
    )

    log.info("loading update data")
    val projectConverter = new ProjectConvert(paths, githubDownload)
    val newData = projectConverter(PomsReader.loadAll(paths))

    var count = 0
    for((project, releases) <- newData) {
      val indexProject = esClient.execute {
        indexInto(indexName / projectsCollection).source(project)
      }

      val indexReleases = esClient.execute {
        bulk(
          releases.map(
            release => indexInto(indexName / releasesCollection).source(release)
          )
        )
      }

      val result = for {
        bulkResult <- indexReleases
        _ <- indexProject
      } yield {
        if (bulkResult.hasFailures) {
          bulkResult.failures.foreach(p => log.error(p.failureMessage))
          log.error(s"Indexing releases of ${project.repository} failed")
        }
      }
      Await.result(result, Duration.Inf)
      count += 1
    }

    log.info(s"$count projects indexed")
  }
}
