name := "elastic-sql"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % "0.7.0",
  "org.parboiled" %% "parboiled" % "2.1.4",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

scalacOptions ++= Seq("-feature")
