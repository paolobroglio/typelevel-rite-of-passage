package com.paolobroglio.projects.jobsboard.modules

import cats.effect.*
import cats.syntax.*
import cats.implicits.*
import com.paolobroglio.projects.jobsboard.config.SecurityConfig
import com.paolobroglio.projects.jobsboard.core.{Auth, Jobs, LiveAuth, LiveJobs, LiveUsers}
import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts
import doobie.util.transactor.Transactor
import org.typelevel.log4cats.Logger

final class Core[F[_]] private (val jobs: Jobs[F], val auth: Auth[F])

object Core {
  def apply[F[_]: Async: Logger](xa: Transactor[F])(securityConfig: SecurityConfig): Resource[F, Core[F]] = {
    val coreF = for {
      jobs <- LiveJobs[F](xa)
      users <- LiveUsers[F](xa)
      auth <- LiveAuth[F](users)(securityConfig)
    } yield new Core(jobs, auth)

    Resource.eval(coreF)
  }
}
