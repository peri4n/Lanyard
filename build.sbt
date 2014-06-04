name := "lanyard"

description := "A library for probabilistic programming in scala."

version := "0.0.1"

scalaVersion := "2.11.0"

organization := "org.lanyard"

licenses := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php"))

resolvers += "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"

libraryDependencies ++= Seq (
		    "org.scalatest" %% "scalatest" % "2.1.7" % "test",
		    "org.scalacheck" %% "scalacheck" % "1.11.4" % "test"
)

scalacOptions ++= Seq ("-unchecked", "-feature", "-deprecation", "-optimize" )

crossScalaVersions := Seq("2.10.4", "2.11.0")

publishMavenStyle := true

publishTo := Some(Resolver.sftp("My Private Repo", "ftp.mybioinformatics.org", "repos/lanyard"))
