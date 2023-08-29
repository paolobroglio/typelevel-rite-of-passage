package com.paolobroglio.projects.jobsboard.modules

import cats.effect.*
import com.paolobroglio.projects.jobsboard.core.{Jobs, LiveJobs}
import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts
import doobie.util.transactor.Transactor

final class Core[F[_]] private (val jobs: Jobs[F])

object Core {
  def apply[F[_]: Async](xa: Transactor[F]): Resource[F, Core[F]] =
    Resource
      .eval(LiveJobs[F](xa))
      .map(jobs => new Core[F](jobs))
}