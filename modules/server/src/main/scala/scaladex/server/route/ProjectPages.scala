package scaladex.server.route

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

import akka.http.scaladsl.model.Uri._
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import com.typesafe.scalalogging.LazyLogging
import play.twirl.api.HtmlFormat
import scaladex.core.model.Artifact
import scaladex.core.model.ArtifactSelection
import scaladex.core.model.Category
import scaladex.core.model.Env
import scaladex.core.model.GithubStatus
import scaladex.core.model.Platform
import scaladex.core.model.Project
import scaladex.core.model.SemanticVersion
import scaladex.core.model.UserState
import scaladex.core.service.Storage
import scaladex.core.service.WebDatabase
import scaladex.server.TwirlSupport._
import scaladex.view

class ProjectPages(env: Env, database: WebDatabase, localStorage: Storage)(
    implicit executionContext: ExecutionContext
) extends LazyLogging {

  def route(user: Option[UserState]): Route =
    concat(
      post {
        path("edit" / organizationM / repositoryM) { (organization, repository) =>
          editForm { form =>
            val ref = Project.Reference(organization, repository)
            val updateF = database.updateProjectSettings(ref, form)
            onComplete(updateF) {
              case Success(()) =>
                redirect(
                  Uri(s"/$organization/$repository"),
                  StatusCodes.SeeOther
                )
              case Failure(e) =>
                logger.error(s"Cannot save settings of project $ref", e)
                redirect(
                  Uri(s"/$organization/$repository"),
                  StatusCodes.SeeOther
                ) // maybe we can print that it wasn't saved
            }
          }
        }
      },
      get {
        path("artifacts" / organizationM / repositoryM) { (org, repo) =>
          val ref = Project.Reference(org, repo)
          val res =
            for {
              projectOpt <- database.getProject(ref)
              project = projectOpt.getOrElse(throw new Exception(s"project ${ref} not found"))
              artifacts <- database.getArtifacts(project.reference)
            } yield (project, artifacts)

          onComplete(res) {
            case Success((project, artifacts)) =>
              val platformByVersions = artifacts
                .map(_.platform)
                .distinct
                .groupBy(_.version)
                .view
                .mapValues(_.sorted(Platform.ordering.reverse))
                .toSeq
                .sortBy(_._1)(Platform.Version.ordering.reverse)

              val artifactsByVersions = artifacts
                .groupBy(_.version)
                .map { case (version, artifacts) => (version, artifacts.groupBy(_.artifactName).toSeq.sortBy(_._1)) }
                .toSeq
                .sortBy(_._1)(SemanticVersion.ordering.reverse)

              complete(view.html.artifacts(env, project, user, platformByVersions, artifactsByVersions))
            case Failure(e) =>
              complete(StatusCodes.NotFound, view.html.notfound(env, user))
          }
        }
      },
      get {
        path("edit" / organizationM / repositoryM) { (organization, repository) =>
          val projectRef = Project.Reference(organization, repository)
          user match {
            case Some(userState) if userState.canEdit(projectRef) =>
              complete(getEditPage(projectRef, userState))
            case maybeUser =>
              complete((StatusCodes.Forbidden, view.html.forbidden(env, maybeUser)))
          }
        }
      },
      get {
        path(organizationM / repositoryM)((organization, repository) =>
          parameters("artifact".?, "version".?, "target".?, "selected".?) { (artifact, version, target, selected) =>
            val projectRef = Project.Reference(organization, repository)
            val fut: Future[StandardRoute] = database.getProject(projectRef).flatMap {
              case Some(Project(_, _, _, GithubStatus.Moved(_, newProjectRef), _, _)) =>
                Future.successful(redirect(Uri(s"/$newProjectRef"), StatusCodes.PermanentRedirect))
              case Some(project) =>
                val artifactRouteF: Future[StandardRoute] =
                  getSelectedArtifact(
                    database,
                    project,
                    platform = target,
                    artifact = artifact.map(Artifact.Name.apply),
                    version = version,
                    selected = selected
                  ).map(_.map { artifact =>
                    val targetParam = s"?target=${artifact.platform.encode}"
                    redirect(
                      s"/$organization/$repository/${artifact.artifactName}/${artifact.version}/$targetParam",
                      StatusCodes.TemporaryRedirect
                    )
                  }.getOrElse(complete(StatusCodes.NotFound, view.html.notfound(env, user))))
                artifactRouteF
              case None =>
                Future.successful(
                  complete(StatusCodes.NotFound, view.html.notfound(env, user))
                )
            }
            onSuccess(fut)(identity)
          }
        )
      },
      get {
        path(organizationM / repositoryM / artifactM)((organization, repository, artifact) =>
          parameter("target".?) { target =>
            val res = getProjectPage(
              organization,
              repository,
              target,
              artifact,
              None,
              user
            )
            onComplete(res) {
              case Success((code, some)) => complete(code, some)
              case Failure(e) =>
                complete(StatusCodes.NotFound, view.html.notfound(env, user))
            }
          }
        )
      },
      get {
        path(organizationM / repositoryM / artifactM / versionM)((organization, repository, artifact, version) =>
          parameter("target".?) { target =>
            val res = getProjectPage(organization, repository, target, artifact, Some(version), user)
            onComplete(res) {
              case Success((code, some)) => complete(code, some)
              case Failure(e) =>
                complete(StatusCodes.NotFound, view.html.notfound(env, user))
            }
          }
        )
      }
    )

  private def getEditPage(ref: Project.Reference, user: UserState): Future[(StatusCode, HtmlFormat.Appendable)] =
    for {
      projectOpt <- database.getProject(ref)
      artifacts <- database.getArtifacts(ref)
    } yield projectOpt
      .map { p =>
        val page = view.project.html.editproject(env, p, artifacts, Some(user))
        (StatusCodes.OK, page)
      }
      .getOrElse((StatusCodes.NotFound, view.html.notfound(env, Some(user))))

  private def filterVersions(p: Project, allVersions: Seq[SemanticVersion]): Seq[SemanticVersion] =
    (if (p.settings.strictVersions) allVersions.filter(_.isSemantic)
     else allVersions).distinct.sorted.reverse

  private def getProjectPage(
      organization: Project.Organization,
      repository: Project.Repository,
      target: Option[String],
      artifact: Artifact.Name,
      version: Option[SemanticVersion],
      user: Option[UserState]
  ): Future[(StatusCode, HtmlFormat.Appendable)] = {
    val selection = ArtifactSelection.parse(
      platform = target,
      artifactName = Some(artifact),
      version = version.map(_.toString),
      selected = None
    )
    val projectRef =
      Project.Reference(organization, repository)

    database.getProject(projectRef).flatMap {
      case Some(project) =>
        for {
          artifats <- database.getArtifacts(projectRef)
          selectedArtifact = selection
            .filterArtifacts(artifats, project)
            .headOption
            .getOrElse(throw new Exception(s"no artifact found for $projectRef"))
          directDependencies <- database.getDirectDependencies(selectedArtifact)
          reverseDependency <- database.getReverseDependencies(selectedArtifact)

          allVersions = artifats.map(_.version)
          filteredVersions = filterVersions(project, allVersions)
          platforms = artifats.map(_.platform).distinct.sorted.reverse
          artifactNames = artifats.map(_.artifactName).distinct.sortBy(_.value)
          twitterCard = project.twitterSummaryCard
        } yield (
          StatusCodes.OK,
          view.project.html.project(
            env,
            project,
            artifactNames,
            filteredVersions,
            platforms,
            selectedArtifact,
            user,
            canEdit = true,
            Some(twitterCard),
            artifats.size,
            directDependencies,
            reverseDependency
          )
        )
      case None =>
        Future.successful((StatusCodes.NotFound, view.html.notfound(env, user)))
    }
  }

  // TODO remove all unused parameters
  private val editForm: Directive1[Project.Settings] =
    formFieldSeq.tflatMap(fields =>
      formFields(
        "contributorsWanted".as[Boolean] ? false,
        "defaultArtifact".?,
        "defaultStableVersion".as[Boolean] ? false,
        "strictVersions".as[Boolean] ? false,
        "deprecated".as[Boolean] ? false,
        "artifactDeprecations".as[String].*,
        "cliArtifacts".as[String].*,
        "customScalaDoc".?,
        "category".?,
        "beginnerIssuesLabel".?,
        "selectedBeginnerIssues".as[String].*,
        "chatroom".?,
        "contributingGuide".?,
        "codeOfConduct".?
      ).tmap {
        case (
              contributorsWanted,
              rawDefaultArtifact,
              defaultStableVersion,
              strictVersions,
              deprecated,
              rawArtifactDeprecations,
              rawCliArtifacts,
              rawCustomScalaDoc,
              rawCategory,
              rawBeginnerIssuesLabel,
              selectedBeginnerIssues,
              rawChatroom,
              rawContributingGuide,
              rawCodeOfConduct
            ) =>
          val documentationLinks =
            fields._1
              .filter { case (key, _) => key.startsWith("documentationLinks") }
              .groupBy {
                case (key, _) =>
                  key
                    .drop("documentationLinks[".length)
                    .takeWhile(_ != ']')
              }
              .values
              .map {
                case Vector((a, b), (_, d)) =>
                  if (a.contains("label")) (b, d)
                  else (d, b)
              }
              .flatMap {
                case (label, link) =>
                  Project.DocumentationLink.from(label, link)
              }
              .toList

          def noneIfEmpty(value: String): Option[String] =
            if (value.isEmpty) None else Some(value)

          val settings: Project.Settings = Project.Settings(
            defaultStableVersion,
            rawDefaultArtifact.flatMap(noneIfEmpty).map(Artifact.Name.apply),
            strictVersions,
            rawCustomScalaDoc.flatMap(noneIfEmpty),
            documentationLinks,
            deprecated,
            contributorsWanted,
            rawArtifactDeprecations.map(Artifact.Name.apply).toSet,
            rawCliArtifacts.map(Artifact.Name.apply).toSet,
            rawCategory.flatMap(Category.byLabel.get),
            rawBeginnerIssuesLabel.flatMap(noneIfEmpty)
          )
          Tuple1(settings)
      }
    )
}
