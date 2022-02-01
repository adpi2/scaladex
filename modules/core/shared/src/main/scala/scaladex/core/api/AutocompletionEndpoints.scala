package scaladex.core.api

import endpoints4s.algebra

// Autocompletion endpoints are implemented by the server and invoked by the web client.
// There definition is shared here, for consistency.
trait AutocompletionEndpoints extends algebra.Endpoints with algebra.JsonEntitiesFromSchemas {

  /** Enrich a request with session information */
  def withOptionalSession[A](request: Request[A]): Request[WithSession[A]]

  /** A type `A` enriched with session information (unknown yet, defined by the web client and server) */
  type WithSession[A]

  // JSON schema of the autocompletion response entity
  implicit val response: JsonSchema[AutocompletionResponse] =
    field[String]("organization")
      .zip(field[String]("repository"))
      .zip(field[String]("description"))
      .xmap[AutocompletionResponse] {
        case (organization, repository, description) => AutocompletionResponse(organization, repository, description)
      } { autocompletionResponse =>
        (autocompletionResponse.organization, autocompletionResponse.repository, autocompletionResponse.description)
      }

  // Definition of the autocompletion query format
  val searchRequestQuery: QueryString[AutocompletionRequest] = (
    qs[String]("q", docs = Some("Main query (e.g., 'json', 'testing', etc.)")) &
      qs[Option[String]]("you", docs = Some("Used internally by Scaladex web user interface"))
        .xmap[Boolean](_.contains("✓"))(Option.when(_)("✓")) &
      qs[Seq[String]]("topics", docs = Some("Filter the results matching the given topics only")) &
      qs[Seq[String]](
        "scalaVersions",
        docs =
          Some("Filter the results matching the given Scala versions only (e.g., 'scala3', '2.13', '2.12', '2.11')")
      ) &
      qs[Seq[String]](
        "platformVersions",
        docs =
          Some("Filter the results matching the given platform versions only (e.g., 'jvm', 'js1', 'native0.4', 'sbt1')")
      ) &
      qs[Option[Boolean]]("contributingSearch").xmap(_.getOrElse(false))(Option.when(_)(true))
  ).xmap((AutocompletionRequest.apply _).tupled)(Function.unlift(AutocompletionRequest.unapply))

  // Autocomplete endpoint definition
  val autocomplete: Endpoint[WithSession[AutocompletionRequest], Seq[AutocompletionResponse]] =
    endpoint(
      withOptionalSession(get(path / "api" / "autocomplete" /? searchRequestQuery)),
      ok(jsonResponse[Seq[AutocompletionResponse]])
    )

}
