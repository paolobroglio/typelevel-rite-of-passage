package com.paolobroglio.projects.jobsboard.config

import pureconfig.ConfigReader
import pureconfig.generic.derivation.default.*
import scala.concurrent.duration.FiniteDuration

case class SecurityConfig (secret: String, jwtExpirationDuration: FiniteDuration) derives ConfigReader
