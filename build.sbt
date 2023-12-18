import ScalaJSHelper._
import Deployment.githash

lazy val isCI: Boolean = System.getenv("CI") != null

inThisBuild(
  List(
    scalaVersion := "2.13.10",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalafixScalaBinaryVersion := "2.13",
    scalafixDependencies ++= List(
      "com.github.liancheng" %% "organize-imports" % "0.6.0"
    ),
    organization := "ch.epfl.scala",
    version := s"0.2.0+${githash()}"
  )
)

lazy val loggingSettings = Seq(
  libraryDependencies ++= Seq(
    "ch.qos.logback" % "logback-classic" % "1.3.14",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
  ),
  // Drop and replace commons-logging with slf4j
  libraryDependencies += "org.slf4j" % "jcl-over-slf4j" % "2.0.9",
  excludeDependencies += ExclusionRule("commons-logging", "commons-logging")
)

lazy val scalacOptionsSettings = Def.settings(
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-unchecked",
    "-Wunused"
  ) ++ { if (isCI) Some("-Xfatal-warnings") else None }
)

lazy val scaladex = project
  .in(file("."))
  .aggregate(webclient, data, core.jvm, core.js, infra, server, template)
  .settings(Deployment(data, server))

lazy val template = project
  .in(file("modules/template"))
  .settings(
    scalacOptionsSettings,
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "com.github.nscala-time" %% "nscala-time" % V.nscalaTime,
      "com.typesafe" % "config" % "1.4.3",
      "com.typesafe.akka" %% "akka-http-core" % V.akkaHttp,
      "org.scalatest" %% "scalatest" % V.scalatest % Test
    )
  )
  .dependsOn(core.jvm % "compile->compile;test->test")
  .enablePlugins(SbtTwirl)

lazy val infra = project
  .in(file("modules/infra"))
  .configs(IntegrationTest)
  .settings(
    scalacOptionsSettings,
    loggingSettings,
    libraryDependencies ++= Seq(
      "com.sksamuel.elastic4s" %% "elastic4s-client-esjava" % V.elastic4s,
      "org.flywaydb" % "flyway-core" % "8.5.13", // for database migration
      "com.typesafe.akka" %% "akka-stream" % V.akka,
      "com.typesafe.akka" %% "akka-http" % V.akkaHttp,
      "de.heikoseeberger" %% "akka-http-circe" % "1.39.2",
      "io.get-coursier" %% "coursier" % V.coursier,
      "io.get-coursier" %% "coursier-sbt-maven-repository" % V.coursier,
      "org.tpolecat" %% "doobie-scalatest" % V.doobie % Test,
      "org.scalatest" %% "scalatest" % V.scalatest % "test,it"
    ) ++ Seq(
      "org.tpolecat" %% "doobie-core",
      "org.tpolecat" %% "doobie-h2",
      "org.tpolecat" %% "doobie-postgres",
      "org.tpolecat" %% "doobie-hikari"
    ).map(_ % V.doobie) ++ Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % V.circe),
    Elasticsearch.settings(defaultPort = 9200),
    inConfig(Compile)(
      Postgres.settings(defaultPort = 5432, database = "scaladex")
    ),
    javaOptions ++= {
      val base = (ThisBuild / baseDirectory).value
      val index = base / "small-index"
      val contrib = base / "contrib"
      Seq(
        s"-Dscaladex.filesystem.index=$index",
        s"-Dscaladex.filesystem.contrib=$contrib"
      )
    },
    Compile / run / javaOptions ++= {
      val elasticsearchPort = startElasticsearch.value
      val postgresPort = (Compile / startPostgres).value
      Seq(
        s"-Dscaladex.database.port=$postgresPort",
        s"-Dscaladex.elasticsearch.port=$elasticsearchPort"
      )
    },
    inConfig(Test)(
      Postgres.settings(defaultPort = 5432, database = "scaladex-test")
    ),
    Test / javaOptions ++= {
      val elasticsearchPort = startElasticsearch.value
      val postgresPort = (Test / startPostgres).value
      Seq(
        s"-Dscaladex.database.port=$postgresPort",
        s"-Dscaladex.database.name=scaladex-test",
        s"-Dscaladex.elasticsearch.index=scaladex-test",
        s"-Dscaladex.elasticsearch.port=$elasticsearchPort"
      )
    },
    Test / fork := true,
    // testing the database requests need to delete and create the tables,
    // which can fail if many tests are running in parallel
    Test / parallelExecution := false,
    Defaults.itSettings,
    IntegrationTest / fork := true,
    IntegrationTest / javaOptions ++= (Test / javaOptions).value
  )
  .dependsOn(core.jvm % "compile->compile;test->test;it->test")

lazy val webclient = project
  .in(file("modules/webclient"))
  .settings(
    scalacOptionsSettings,
    scalacOptions -= "-Wunused", // don't report unused params
    scalacOptions += "-Wunused:imports",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "scalatags" % "0.11.1",
      "org.endpoints4s" %%% "fetch-client" % "3.1.0"
    )
  )
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(core.js)

lazy val server = project
  .in(file("modules/server"))
  .configs(IntegrationTest)
  .settings(
    scalacOptionsSettings,
    javaOptions ++= Seq(
      "-Dcom.sun.management.jmxremote.port=9999",
      "-Dcom.sun.management.jmxremote.authenticate=false",
      "-Dcom.sun.management.jmxremote.ssl=false"
    ),
    loggingSettings,
    packageScalaJS(webclient),
    javaOptions ++= Seq(
      "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=1044"
    ),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
      "com.typesafe.play" %%% "play-json" % V.playJson,
      "org.scalatest" %% "scalatest" % V.scalatest % "test,it",
      "com.typesafe.akka" %% "akka-testkit" % V.akka % "test,it",
      "com.typesafe.akka" %% "akka-slf4j" % V.akka,
      "com.typesafe.akka" %% "akka-serialization-jackson" % V.akka,
      "com.typesafe.akka" %% "akka-actor-typed" % V.akka,
      "com.typesafe.akka" %% "akka-stream-testkit" % V.akka % Test,
      "com.typesafe.akka" %% "akka-http-testkit" % V.akkaHttp % Test,
      "ch.megard" %% "akka-http-cors" % "1.2.0",
      "com.softwaremill.akka-http-session" %% "core" % "0.7.1",
      "com.typesafe.akka" %% "akka-http" % V.akkaHttp,
      "org.endpoints4s" %% "akka-http-server" % "7.2.0",
      "org.webjars" % "bootstrap-sass" % "3.4.1",
      "org.webjars" % "bootstrap-switch" % "3.3.4",
      "org.webjars" % "bootstrap-select" % "1.13.18",
      "org.webjars" % "chartjs" % "3.9.1",
      "org.webjars.npm" % "date-fns" % "2.30.0",
      "org.webjars.npm" % "chartjs-adapter-date-fns" % "3.0.0",
      "org.webjars" % "font-awesome" % "6.4.2",
      "org.webjars" % "jquery" % "3.6.3",
      "org.webjars.bower" % "select2" % "4.0.13"
    ),
    Compile / unmanagedResourceDirectories += (Assets / WebKeys.public).value,
    Compile / resourceGenerators += Def.task(
      Seq((Assets / WebKeys.assets).value)
    ),
    fork := true,
    Compile / run / javaOptions ++= (infra / Compile / run / javaOptions).value,
    reStart / javaOptions ++= (infra / Compile / run / javaOptions).value,
    Test / javaOptions ++= (infra / javaOptions).value,
    Defaults.itSettings,
    IntegrationTest / javaOptions ++= (infra / Compile / run / javaOptions).value
  )
  .dependsOn(template, data, infra, core.jvm % "compile->compile;test->test")
  .enablePlugins(SbtSassify, JavaServerAppPackaging)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .in(file("modules/core"))
  .settings(
    scalacOptionsSettings,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "fastparse" % "2.3.3",
      "io.github.cquiroz" %%% "scala-java-time" % "2.5.0",
      "com.typesafe.play" %%% "play-json" % V.playJson,
      "org.endpoints4s" %%% "algebra" % "1.11.1",
      "org.endpoints4s" %% "json-schema-playjson" % "1.11.1" % Test,
      "org.scalatest" %%% "scalatest" % V.scalatest % Test,
      "org.jsoup" % "jsoup" % "1.16.2"
    ) ++ Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
    ).map(_ % V.circe)
  )

lazy val data = project
  .in(file("modules/data"))
  .settings(
    scalacOptionsSettings,
    loggingSettings,
    libraryDependencies ++= Seq(
      "com.github.nscala-time" %% "nscala-time" % V.nscalaTime,
      "com.typesafe.akka" %% "akka-stream" % V.akka,
      "com.typesafe.akka" %% "akka-actor-typed" % V.akka,
      "com.typesafe.akka" %% "akka-serialization-jackson" % V.akka,
      "com.typesafe.akka" %% "akka-slf4j" % V.akka,
      "org.apache.maven" % "maven-model-builder" % "3.9.5",
      "org.jsoup" % "jsoup" % "1.16.2",
      "org.apache.ivy" % "ivy" % "2.5.2",
      "com.typesafe.akka" %% "akka-http" % V.akkaHttp,
      "org.json4s" %% "json4s-native" % V.json4s,
      "org.scalatest" %% "scalatest" % V.scalatest % Test
    ),
    run / fork := true,
    Compile / run / javaOptions ++= (infra / Compile / run / javaOptions).value,
    Test / javaOptions ++= (infra / javaOptions).value
  )
  .enablePlugins(JavaAppPackaging)
  .dependsOn(core.jvm % "compile->compile;test->test", infra)

lazy val V = new {
  val doobie = "0.13.4"
  val playJson = "2.9.4"
  val akka = "2.6.18"
  val akkaHttp = "10.2.10"
  val elastic4s = "8.9.5"
  val nscalaTime = "2.32.0"
  val scalatest = "3.2.17"
  val circe = "0.14.6"
  val json4s = "4.0.6"
  val coursier = "2.1.6"
}
