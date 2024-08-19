package scaladex.infra.sql

import java.time.Instant

import doobie._
import doobie.util.update.Update
import scaladex.core.model.Artifact
import scaladex.core.model.Language
import scaladex.core.model.Platform
import scaladex.core.model.Project
import scaladex.core.model.SemanticVersion
import scaladex.infra.sql.DoobieUtils.Mappings._
import scaladex.infra.sql.DoobieUtils._

object ArtifactTable {
  private[sql] val table = "artifacts"

  val mavenReferenceFields: Seq[String] = Seq("group_id", "artifact_id", "version")
  val projectReferenceFields: Seq[String] = Seq("organization", "repository")

  // Don't use `select *...` but `select fields`
  // the table have more fields than in Artifact instance
  private[sql] val fields = mavenReferenceFields ++
    Seq("artifact_name") ++
    projectReferenceFields ++
    Seq(
      "description",
      "release_date",
      "resolver",
      "licenses",
      "is_non_standard_Lib",
      "platform",
      "language_version",
      "full_scala_version",
      "scaladoc_url",
      "version_scheme",
      "developers"
    )
  // these field are usually excluded when we read artifacts from the artifacts table.
  val versionFields: Seq[String] = Seq("is_semantic", "is_prerelease")
  val isLatestVersion: String = "is_latest_version"

  def insertIfNotExist(artifact: Artifact): ConnectionIO[Int] =
    insertIfNotExist.run((artifact, artifact.version.isSemantic, artifact.version.isPreRelease))

  def insertIfNotExist(artifacts: Seq[Artifact]): ConnectionIO[Int] =
    insertIfNotExist.updateMany(artifacts.map(a => (a, a.version.isSemantic, a.version.isPreRelease)))

  private[sql] val insertIfNotExist: Update[(Artifact, Boolean, Boolean)] =
    insertOrUpdateRequest(table, fields ++ versionFields, mavenReferenceFields)

  val count: Query0[Long] =
    selectRequest(table, Seq("COUNT(*)"))

  val countVersionsByProject: Query[Project.Reference, Long] =
    selectRequest(table, Seq("Count(DISTINCT version)"), projectReferenceFields)

  val updateProjectRef: Update[(Project.Reference, Artifact.MavenReference)] =
    updateRequest(table, projectReferenceFields, mavenReferenceFields)

  val updateReleaseDate: Update[(Instant, Artifact.MavenReference)] =
    updateRequest(table, Seq("release_date"), mavenReferenceFields)

  def selectAllArtifacts(language: Option[Language], platform: Option[Platform]): Query0[Artifact] = {
    val where = language.map(v => s"language_version='${v.label}'").toSeq ++ platform.map(p => s"platform='${p.label}'")
    selectRequest(table, fields, where = where)
  }

  val selectArtifactByGroupIdAndArtifactId: Query[(Artifact.GroupId, String), Artifact] =
    selectRequest(table, fields, Seq("group_id", "artifact_id"))

  val selectArtifactByProject: Query[Project.Reference, Artifact] =
    selectRequest1(
      table,
      fields,
      where = projectReferenceFields.map(f => s"$f=?"),
      orderBy = Some("release_date DESC"),
      limit = Some(10000)
    )

  val selectArtifactBy: Query[(Project.Reference, Artifact.Name, SemanticVersion), Artifact] =
    selectRequest1(
      table,
      fields,
      where = Seq("organization=?", "repository=?", "artifact_name=?", "version=?")
    )

  def selectArtifactByParams(preReleases: Boolean): Query[(Project.Reference, Artifact.Name), Artifact] = {
    val preReleaseFilter = if (preReleases) "true" else "is_prerelease=false"
    Query[(Project.Reference, Artifact.Name), Artifact](
      s"""|SELECT ${fields.mkString(", ")}
          |FROM $table
          |WHERE organization=? AND repository=? AND artifact_name=? AND $preReleaseFilter
          |""".stripMargin
    )
  }

  val selectArtifactByProjectAndName: Query[(Project.Reference, Artifact.Name), Artifact] =
    selectRequest(table, fields, projectReferenceFields :+ "artifact_name")

  val selectByMavenReference: Query[Artifact.MavenReference, Artifact] =
    selectRequest(table, fields, mavenReferenceFields)

  val selectGroupIds: Query0[Artifact.GroupId] =
    selectRequest(table, Seq("DISTINCT group_id"))

  val selectArtifactIds: Query[Project.Reference, (Artifact.GroupId, String)] =
    selectRequest(table, Seq("DISTINCT group_id", "artifact_id"), keys = projectReferenceFields)

  val selectMavenReference: Query0[Artifact.MavenReference] =
    selectRequest(table, Seq("DISTINCT group_id", "artifact_id", "\"version\""))

  val selectMavenReferenceWithNoReleaseDate: Query0[Artifact.MavenReference] =
    selectRequest(table, Seq("group_id", "artifact_id", "\"version\""), where = Seq("release_date is NULL"))

  val selectOldestByProject: Query0[(Instant, Project.Reference)] =
    selectRequest(
      table,
      Seq("MIN(release_date)", "organization", "repository"),
      where = Seq("release_date IS NOT NULL"),
      groupBy = projectReferenceFields
    )

  def selectLatestArtifacts: Query[Project.Reference, Artifact] =
    selectRequest1(table, fields, where = Seq("organization=?", "repository=?", "is_latest_version=true"))

  def setLatestVersion: Update[Artifact.MavenReference] =
    updateRequest0(table, set = Seq("is_latest_version=true"), where = Seq("group_id=?", "artifact_id=?", "version=?"))

  def unsetOthersLatestVersion: Update[Artifact.MavenReference] =
    updateRequest0(
      table,
      set = Seq("is_latest_version=false"),
      where = Seq("group_id=?", "artifact_id=?", "version<>?")
    )
}
