package com.paolobroglio.projects.jobsboard.modules

import cats.effect.{Async, Resource}
import com.paolobroglio.projects.jobsboard.config.PostgresqlConfig
import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts

object Database {
  def makePostgresResource[F[_] : Async](config: PostgresqlConfig): Resource[F, HikariTransactor[F]] =
    for {
      ec <- ExecutionContexts.fixedThreadPool(config.threads)
      xa <- HikariTransactor.newHikariTransactor[F](
        "org.postgresql.Driver",
        config.host,
        config.username,
        config.password,
        ec
      )
    } yield xa
}
