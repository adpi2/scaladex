package scaladex.core.model.search

import scaladex.core.model.Platform
import scaladex.core.model.Project

case class SearchParams(
    queryString: String = "",
    page: Int = 0,
    sorting: Option[String] = None,
    userRepos: Set[Project.Reference] = Set(),
    total: Int = 20,
    targetFiltering: Option[Platform] = None,
    cli: Boolean = false,
    topics: Seq[String] = Nil,
    scalaVersions: Seq[String] = Nil,
    platformVersions: Seq[String] = Nil,
    contributingSearch: Boolean = false
)
