lazy val commonSettings = Seq(
  scalaVersion := "2.11.1",
  scalacOptions += "-deprecation",
  libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.3",
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M1" cross CrossVersion.full)
)

lazy val macros = project.settings(
  commonSettings : _*
).settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

lazy val sample = project.settings(
  commonSettings : _*
).settings(
).dependsOn(macros)

lazy val root = project.in(file(".")).settings(
  commonSettings : _*
).aggregate(macros, sample)
