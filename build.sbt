import sbt._
import Keys._

val commonSettings = Seq(
    version := "1.0.0",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    scalacOptions ++= Seq("-Ymacro-annotations", "-language:experimental.macros"),
    scalaVersion := "2.13.1",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.1"
)
val macroSetting = commonSettings
lazy val macros: Project = project.in(file("macros"))
  .settings(macroSetting)
  .settings(
      name := "macros"
  )
lazy val reflectTest: Project = project.in(file("main"))
  .settings(commonSettings)
  .dependsOn(macros)
  .settings(
      name := "reflect-test"
  )

lazy val root = project.in(file("."))
  .aggregate(macros, reflectTest)
  .settings(commonSettings)
