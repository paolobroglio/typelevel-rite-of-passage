package com.paolobroglio.projects.jobsboard.http.routes

import cats.Monad
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router

class HealthRoutes[F[_]: Monad] private extends Http4sDsl[F] {
  private val healthRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root =>
      Ok("UP!")
  }

  val routes: HttpRoutes[F] = Router(
    "/health" -> healthRoute
  )
}

object HealthRoutes {
  def apply[F[_]: Monad] = new HealthRoutes[F]
}