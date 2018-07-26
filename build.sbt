name := """epi"""

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.scalactic" %% "scalactic" % "3.0.5"
)
Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-o", "-e")
scalacOptions ++= Seq("unchecked", "deprecation")
