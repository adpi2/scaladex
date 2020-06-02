package ch.epfl.scala.index.data
package bintray

import java.net.URL
import java.nio.file.{Files, Path}

import akka.actor.ActorSystem
import akka.stream.Materializer
import ch.epfl.scala.index.data.download.PlayWsClient
import jawn.support.json4s.Parser
import org.json4s.JsonAST.JValue
import play.api.libs.ws.{WSAuthScheme, WSClient, WSRequest, WSResponse}
import resource.ManagedResource

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class BintrayClient(credentials: Path,
                    val client: WSClient)(implicit ec: ExecutionContext)
    extends BintrayProtocol {
  import BintrayClient._

  val bintrayCredentials = {
    // from bintray-sbt convention
    // cat ~/.bintray/.credentials
    // host = api.bintray.com
    // user = xxxxxxxxxx
    // password = xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

    if (Files.exists(credentials) && Files.isDirectory(credentials)) {
      val source = scala.io.Source.fromFile(
        credentials.resolve("search-credentials").toFile
      )

      val info = source.mkString
        .split('\n')
        .map { v =>
          val (l, r) = v.span(_ != '=')
          (l.trim, r.drop(2).trim)
        }
        .toMap
      source.close()
      info
    } else Map[String, String]()
  }

  val bintrayBase: String = "https://bintray.com"

  /** Base URL of Bintray API */
  val apiUrl: String = s"$bintrayBase/api/v1"

  def withAuth(request: WSRequest): WSRequest = {
    (bintrayCredentials.get("user"), bintrayCredentials.get("password")) match {
      case (Some(user), Some(password)) =>
        request.withAuth(user, password, WSAuthScheme.BASIC)
      case _ => request
    }
  }

  // See https://bintray.com/docs/usermanual/downloads/downloads_downloadingusingapis.html#_overview
  def downloadUrl(subject: String, repo: String, path: String): URL =
    new URL(s"https://dl.bintray.com/$subject/$repo/$path")

  /**
   * Fetches a bintray-paginated resource.
   *
   * @param fetchPage Function that fetches one page given the index of the first element to fetch
   * @param decode    Function that decodes the response into a meaningful list of data
   * @return The whole resource
   */
  def fetchPaginatedResource[A](
      fetchPage: Int => Future[WSResponse]
  )(
      decode: WSResponse => Seq[A]
  )(implicit ec: ExecutionContext): Future[Seq[A]] = {
    for {
      // Let’s first get the first page
      firstResponse <- fetchPage(0)
      // And then get the remaining pages, if any
      remainingResponses <- Future.traverse(remainingPages(firstResponse))(
        fetchPage
      )
    } yield {
      // Eventually concatenate all the results together
      remainingResponses.foldLeft(decode(firstResponse)) {
        (results, otherResults) =>
          results ++ decode(otherResults)
      }
    }
  }

  /**
   * @param response The HTTP response that we want to decode
   * @param decode   The function that decodes the JSON content into a list of meaningful information
   * @return The decoded content. In case of (any) failure, logs the error and returns an empty list.
   */
  def decodeSucessfulJson[A](decode: JValue => A)(response: WSResponse): A = {
    if (response.status != 200) {
      sys.error(
        s"Got a response with a non-OK status: ${response.statusText} ${response.body}"
      )
    }
    decode(Parser.parseUnsafe(response.body))
  }
}

object BintrayClient {

  def create(credentials: Path)(
      implicit mat: Materializer,
      sys: ActorSystem
  ): ManagedResource[BintrayClient] = {
    for (client <- PlayWsClient.open())
      yield new BintrayClient(credentials, client)(sys.dispatcher)
  }

  /**
   * @return The list of the remaining queries that have to be performed to get the missing packages
   * @param response Response of the ''first'' query
   */
  def remainingPages(response: WSResponse): Seq[Int] = {
    val remainingPages = for {
      total <- response
        .header("X-RangeLimit-Total")
        .flatMap(s => Try(s.toInt).toOption)
      startPos <- response
        .header("X-RangeLimit-StartPos")
        .flatMap(s => Try(s.toInt).toOption)
      endPos <- response
        .header("X-RangeLimit-EndPos")
        .flatMap(s => Try(s.toInt).toOption)
      if endPos < (total - 1)
      nextPos = endPos + 1
      perPage = nextPos - startPos
      remainingPageCount = Math
        .ceil((total - nextPos).toDouble / perPage)
        .toInt
    } yield Seq.tabulate(remainingPageCount)(page => nextPos + page * perPage)
    remainingPages.getOrElse(Seq())
  }
}
