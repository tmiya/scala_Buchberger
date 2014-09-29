organization := "tmiya"

name := "Algebra"

version := "0.1.0"

scalaVersion := "2.11.0"

// parallelExecution in Test := false

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"  
)

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.4.4" % "test",
  "org.scalaz.stream" %% "scalaz-stream" % "0.5"
)

scalacOptions ++= Seq("-encoding", "UTF-8")

javacOptions ++= Seq("-encoding", "UTF-8")

// Specs2:JUnit:XmlReport
testOptions in Test += Tests.Argument("junitxml", "console")

publishArtifact in (Compile, packageDoc) := false

