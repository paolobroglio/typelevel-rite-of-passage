package com.paolobroglio.projects.jobsboard

import cats.Monad
import cats.effect.{IO, IOApp, Resource}
import com.paolobroglio.projects.jobsboard.config.{ApplicationConfig, EmberConfig}
import com.paolobroglio.projects.jobsboard.config.syntax.loadF
import com.paolobroglio.projects.jobsboard.core.Jobs
import com.paolobroglio.projects.jobsboard.http.routes.HealthRoutes
import com.paolobroglio.projects.jobsboard.modules.*
import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import pureconfig.ConfigSource
import pureconfig.error.ConfigReaderException

object Application extends IOApp.Simple {

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  override def run: IO[Unit] = ConfigSource.default.loadF[IO, ApplicationConfig].flatMap {
    case ApplicationConfig(postgresqlConfig, emberConfig, securityConfig) =>
      val appResource = for {
        database <- Database.makePostgresResource[IO](postgresqlConfig)
        core <- Core[IO](database)
        httpApi <- HttpApi[IO](core, securityConfig)
        server <- EmberServerBuilder
          .default[IO]
          .withHost(emberConfig.host)
          .withPort(emberConfig.port)
          .withHttpApp(httpApi.endpoints.orNotFound)
          .build
      } yield server

      appResource.use(_ => IO.println("Server is running!") *> IO.never)
  }
}
