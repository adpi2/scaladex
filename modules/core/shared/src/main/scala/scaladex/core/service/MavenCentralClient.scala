package scaladex.core.service

import java.time.Instant

import scala.concurrent.Future

import scaladex.core.model.Artifact
import scaladex.core.model.SemanticVersion

trait MavenCentralClient {
  def getAllArtifactIds(groupId: Artifact.GroupId): Future[Seq[Artifact.ArtifactId]]
  def getAllVersions(groupId: Artifact.GroupId, artifactId: Artifact.ArtifactId): Future[Seq[SemanticVersion]]
  def getPomFile(mavenReference: Artifact.Reference): Future[Option[(String, Instant)]]
}
