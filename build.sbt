cancelable in Global := true

lazy val commonSettings =  Seq(
    organization := "scalaeff"
  , name := "scalaeff"
  , version := "0.1.0-SNAPSHOT"
  , scalaVersion := "2.11.7"
  , logLevel in update := Level.Warn
)

lazy val strictScalac =
  scalacOptions ++= Seq(
    "-Yrangepos"
    , "-Xlint"
    ,"-deprecation"
    , "-Xfatal-warnings"
    , "-feature"
    , "-encoding", "UTF-8"
    //, "-unchecked"
    , "-Yno-adapted-args"
    , "-Ywarn-dead-code"
    , "-Ywarn-numeric-widen"
    , "-Ywarn-value-discard"
    , "-Xfuture"
    //, "-Ywarn-unused-import"
)


lazy val root = project.in(file("."))
  .settings(commonSettings:_*)
  .settings(name := "scalaeff")
  .settings(
    libraryDependencies ++= Seq(
        "com.chuusai"     %%  "shapeless"        % "2.2.4"
      , "org.scalatest"   %%  "scalatest"        % "2.2.1"   % "test"
      , "org.spire-math"  %%  "cats"             % "0.2.0"
      , "org.typelevel"   %%  "export-hook"      % "1.1.0"
      , "org.scala-lang"  % "scala-reflect"      % scalaVersion.value % "provided"
      , compilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
    )
  )
