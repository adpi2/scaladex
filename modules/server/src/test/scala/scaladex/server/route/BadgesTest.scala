package scaladex.server.route

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.headers.Location
import akka.http.scaladsl.server.Route
import org.scalatest.BeforeAndAfterAll
import scaladex.core.model.ScalaVersion._
import scaladex.core.test.Values._
import scaladex.core.util.ScalaExtensions._

class BadgesTest extends ControllerBaseSuite with BeforeAndAfterAll {

  val badgesRoute: Route = new Badges(database).route

  override protected def beforeAll(): Unit = Await.result(insertCats(), Duration.Inf)

  def insertCats(): Future[Unit] =
    Cats.allArtifacts.map(database.insertArtifact(_, Seq.empty, now)).sequence.map(_ => ())

  it("should provide a concise summary of latest versions") {
    Badges.summaryOfLatestVersions(
      Map(
        `2.11` -> Seq(`7.0.0`, `7.1.0`),
        `2.12` -> Seq(`7.0.0`, `7.1.0`, `7.2.0`),
        `2.13` -> Seq(`7.0.0`, `7.1.0`, `7.2.0`, `7.3.0`),
        `3` -> Seq(`7.2.0`, `7.3.0`)
      )
    ) shouldBe "7.3.0 (Scala 3.x, 2.13), 7.2.0 (Scala 2.12), 7.1.0 (Scala 2.11)"
  }

  it("should fallback to JVM artifacts") {
    Get(s"/${Cats.reference}/cats-core/latest-by-scala-version.svg") ~> badgesRoute ~> check {
      status shouldEqual StatusCodes.TemporaryRedirect
      val redirection = headers.collectFirst { case Location(uri) => uri }
      redirection should contain(
        Uri("https://img.shields.io/badge/cats--core_--_JVM-2.7.0_(Scala_3.x)-green.svg?")
      )
    }
  }

  it("should fallback to sjs1 when targetType is js") {
    Get(s"/${Cats.reference}/cats-core/latest-by-scala-version.svg?targetType=js") ~> badgesRoute ~> check {
      status shouldEqual StatusCodes.TemporaryRedirect
      val redirection = headers.collectFirst { case Location(uri) => uri }
      redirection should contain(
        Uri("https://img.shields.io/badge/cats--core_--_Js_1.x-2.6.1_(Scala_3.x)-green.svg?")
      )
    }
  }

  it("should return latest version for Scala.js 0.6") {
    Get(s"/${Cats.reference}/cats-core/latest-by-scala-version.svg?platformVersion=sjs0.6") ~> badgesRoute ~> check {
      status shouldEqual StatusCodes.TemporaryRedirect
      val redirection = headers.collectFirst { case Location(uri) => uri }
      redirection should contain(
        Uri("https://img.shields.io/badge/cats--core_--_Js_0.6-2.6.1_(Scala_2.13)-green.svg?")
      )
    }
  }

  it("should return latest version for Scala native 0.4") {
    Get(s"/${Cats.reference}/cats-core/latest-by-scala-version.svg?platformVersion=native0.4") ~> badgesRoute ~> check {
      status shouldEqual StatusCodes.TemporaryRedirect
      val redirection = headers.collectFirst { case Location(uri) => uri }
      redirection should contain(
        Uri("https://img.shields.io/badge/cats--core_--_Native_0.4-2.6.1_(Scala_2.13)-green.svg?")
      )
    }
  }
}
