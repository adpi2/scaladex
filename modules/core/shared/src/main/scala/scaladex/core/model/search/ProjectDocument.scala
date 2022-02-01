package scaladex.core.model.search

import java.time.Instant

import scaladex.core.model.Artifact
import scaladex.core.model.Category
import scaladex.core.model.Platform
import scaladex.core.model.Project
import scaladex.core.model.ScalaVersion

// Project document indexed by the search engine
final case class ProjectDocument(
    organization: Project.Organization,
    repository: Project.Repository,
    artifactNames: Seq[Artifact.Name],
    hasCli: Boolean,
    creationDate: Option[Instant],
    updateDate: Option[Instant],
    scalaVersions: Seq[ScalaVersion], // scala version families TODO move to BinaryVersion
    platformVersions: Seq[Platform.Version],
    inverseProjectDependencies: Int,
    category: Option[Category],
    formerReferences: Seq[Project.Reference],
    githubInfo: Option[GithubInfoDocument]
) {
  def reference: Project.Reference = Project.Reference(organization, repository)
  def id: String = reference.toString
  def scalaJsVersions: Seq[Platform.Version.Js] = platformVersions.collect { case v @ Platform.Version.Js(_) => v }
  def scalaNativeVersions: Seq[Platform.Version.Native] = platformVersions.collect {
    case v @ Platform.Version.Native(_) => v
  }
  def sbtVersions: Seq[Platform.Version.Sbt] = platformVersions.collect { case v @ Platform.Version.Sbt(_) => v }
}

object ProjectDocument {
  def apply(
      project: Project,
      artifacts: Seq[Artifact],
      inverseProjectDependencies: Int,
      formerReferences: Seq[Project.Reference]
  ): ProjectDocument = {
    import project._
    val platforms = artifacts.map(_.platform)
    ProjectDocument(
      organization,
      repository,
      artifacts.map(_.artifactName).sorted.distinct,
      hasCli,
      creationDate,
      updateDate = None,
      platforms.flatMap(_.scalaVersion).sorted.distinct,
      platforms.map(_.version).sorted.distinct,
      inverseProjectDependencies,
      settings.category,
      formerReferences,
      project.githubInfo.map(_.toDocument)
    )
  }

}
