package com.paolobroglio.projects.jobsboard.config

import pureconfig.ConfigReader
import pureconfig.error.CannotConvert
import pureconfig.generic.derivation.default.*
final case class ApplicationConfig (
    postgresqlConfig: PostgresqlConfig,
    emberConfig: EmberConfig,
    securityConfig: SecurityConfig
                                   ) derives ConfigReader
