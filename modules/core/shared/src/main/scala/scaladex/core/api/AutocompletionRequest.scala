package scaladex.core.api

import scaladex.core.model.Project
import scaladex.core.model.UserState
import scaladex.core.model.search.SearchParams

case class AutocompletionRequest(
    query: String,
    you: Boolean,
    topics: Seq[String],
    scalaVersions: Seq[String],
    platformVersions: Seq[String],
    contributingSearch: Boolean
) {
  def searchParams(user: Option[UserState]): SearchParams = {
    val userRepos = if (you) user.map(_.repos).getOrElse(Set.empty) else Set.empty[Project.Reference]
    SearchParams(
      queryString = query,
      page = 1,
      sorting = None,
      userRepos = userRepos,
      topics = topics,
      scalaVersions = scalaVersions,
      platformVersions = platformVersions,
      contributingSearch = contributingSearch
    )
  }
}
