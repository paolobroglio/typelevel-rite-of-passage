package com.paolobroglio.projects.jobsboard.config


import pureconfig.ConfigReader
import pureconfig.error.CannotConvert
import pureconfig.generic.derivation.default.*

final case class PostgresqlConfig(threads: Int, host: String, username: String, password: String) derives ConfigReader
