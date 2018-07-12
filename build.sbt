name := "mechanisms"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.1.0",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)

scalacOptions ++= Seq(
  "-deprecation",  // issues warning if we use any deprecated API features
  "-Ypartial-unification",
  "-Ywarn-dead-code",
)
