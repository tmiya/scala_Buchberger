organization := "tmiya"

name := "Algebra"

version := "0.1.0"

scalaVersion := "2.11.0"

// parallelExecution in Test := false

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
)

scalacOptions ++= Seq("-encoding", "UTF-8")

javacOptions ++= Seq("-encoding", "UTF-8")

// Specs2:JUnit:XmlReport
testOptions in Test += Tests.Argument("junitxml", "console")

publishArtifact in (Compile, packageDoc) := false

