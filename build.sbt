name := "mechanisms"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.1.0"

scalacOptions ++= Seq(
  "-Ypartial-unification"
)
