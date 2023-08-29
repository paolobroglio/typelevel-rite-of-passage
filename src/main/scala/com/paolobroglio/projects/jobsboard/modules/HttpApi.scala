package com.paolobroglio.projects.jobsboard.modules

import cats.effect.*
import cats.implicits.*
import cats.{Monad, MonadThrow}
import com.paolobroglio.projects.jobsboard.core.Jobs
import com.paolobroglio.projects.jobsboard.http.routes.{HealthRoutes, JobRoutes}
import org.http4s.HttpRoutes
import org.http4s.server.Router
import org.typelevel.log4cats.Logger

class HttpApi[F[_]: Concurrent: Logger] private (core: Core[F]) {
  private val healthRoutes = HealthRoutes[F].routes
  private val jobRoutes = JobRoutes[F](core.jobs).routes
  
  val endpoints: HttpRoutes[F] = Router(
    "/api" -> (healthRoutes <+> jobRoutes)
  )
}

object HttpApi {
  def apply[F[_]: Concurrent: Logger](core: Core[F]): Resource[F, HttpApi[F]] =
    Resource.pure(new HttpApi[F](core))
}