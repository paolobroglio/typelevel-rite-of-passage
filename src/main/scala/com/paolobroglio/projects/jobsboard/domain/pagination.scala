package com.paolobroglio.projects.jobsboard.domain


object pagination {
  final case class Pagination(limit: Int, offset: Int)
  object Pagination {
    def defaultPageSize = 20
    def apply(maybeLimit: Option[Int], maybeOffset: Option[Int]) =
      new Pagination(maybeLimit.getOrElse(defaultPageSize), maybeOffset.getOrElse(0))
  }
}
