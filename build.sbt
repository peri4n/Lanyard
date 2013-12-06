name := "Lanyard"

version := "0.01"

scalaVersion := "2.10.0"

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq (
		    "org.scalatest" % "scalatest_2.10" % "2.0.1-SNAP" % "test",
		    "org.scalacheck" %% "scalacheck" % "1.11.0" % "test"
)

scalacOptions ++= Seq ("-unchecked", "-feature", "-deprecation", "-optimize" )
