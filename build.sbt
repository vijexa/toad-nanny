val http4sVersion = "0.21.11"
val circeVersion = "0.13.0"
val catsVersion = "2.2.0"
val catsEffectVersion = "2.2.0"
val refinedVersion = "0.9.18"
val enumeratumVersion = "1.6.1"
val pprintVersion = "0.5.6"

val LogbackVersion = "1.2.3"
val scalaTestVersion = "3.1.0.0-RC2"

lazy val root = (project in file("."))
  .settings(
    organization := "io.github.vijexa",
    name := "toad-nanny",
    version := "1.1.3",
    scalaVersion := "2.13.4",
    scalacOptions += "-Wunused:imports",
    // so that case classes with private constructors
    // will have private .apply and .copy like in Scala 3
    scalacOptions += "-Xsource:3",
    scalacOptions += "-Ymacro-annotations",
    libraryDependencies ++= Seq(
      "org.http4s"      %% "http4s-blaze-client" % http4sVersion,
      "org.http4s"      %% "http4s-circe"        % http4sVersion,

      "io.circe"        %% "circe-generic"       % circeVersion,
      "io.circe"        %% "circe-generic-extras"% circeVersion,
      "io.circe"        %% "circe-parser"        % circeVersion,

      "org.typelevel"   %% "cats-core"           % catsVersion,
      "org.typelevel"   %% "cats-effect"         % catsEffectVersion,

      "ch.qos.logback"  %  "logback-classic"     % LogbackVersion,

      "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion % Test,
      "org.scalatestplus" %% "selenium-2-45"            % scalaTestVersion % Test,
    )
  )