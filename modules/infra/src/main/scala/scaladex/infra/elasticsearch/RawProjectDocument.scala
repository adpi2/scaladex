package scaladex.infra.elasticsearch

import java.time.Instant

import com.sksamuel.elastic4s.Indexable
import io.circe.Codec
import io.circe.Printer
import io.circe.generic.semiauto
import scaladex.core.model.Artifact
import scaladex.core.model.Category
import scaladex.core.model.Platform
import scaladex.core.model.Project
import scaladex.core.model.ScalaVersion
import scaladex.core.model.search.GithubInfoDocument
import scaladex.core.model.search.ProjectDocument

// A RawProjectDocument is a ProjectDocument where values are not yet validated.
// It can contain invalid values that will be filtered when converting to ProjectDocument.
case class RawProjectDocument(
    organization: Project.Organization,
    repository: Project.Repository,
    artifactNames: Seq[Artifact.Name],
    hasCli: Boolean,
    creationDate: Option[Instant],
    updateDate: Option[Instant],
    scalaVersions: Seq[String],
    platformVersions: Seq[String],
    inverseProjectDependencies: Int,
    category: Option[String],
    formerReferences: Seq[Project.Reference],
    githubInfo: Option[GithubInfoDocument]
) {
  def toProjectDocument: ProjectDocument = ProjectDocument(
    organization,
    repository,
    artifactNames,
    hasCli,
    creationDate,
    updateDate,
    scalaVersions.flatMap(ScalaVersion.parse).sorted,
    platformVersions.flatMap(Platform.Version.parse).sorted,
    inverseProjectDependencies,
    category.flatMap(Category.byLabel.get),
    formerReferences,
    githubInfo
  )
}

object RawProjectDocument {
  import scaladex.infra.Codecs._
  import io.circe.syntax._
  implicit val codec: Codec[RawProjectDocument] = semiauto.deriveCodec
  implicit val indexable: Indexable[RawProjectDocument] = rawDocument => Printer.noSpaces.print(rawDocument.asJson)

  def from(project: ProjectDocument): RawProjectDocument = {
    import project._
    RawProjectDocument(
      organization,
      repository,
      artifactNames,
      hasCli,
      creationDate,
      updateDate,
      scalaVersions.map(_.encode),
      platformVersions.map(_.encode),
      inverseProjectDependencies,
      category.map(_.label),
      formerReferences,
      githubInfo
    )
  }
}
