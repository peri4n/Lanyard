name := "lanyard"

version := "0.01"

organization := "org.lanyard"

licenses := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php"))

scalaVersion := "2.10.3"

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq (
		    "org.scalatest" % "scalatest_2.10" % "2.0.1-SNAP" % "test",
		    "org.scalacheck" %% "scalacheck" % "1.11.0" % "test"
)

scalacOptions ++= Seq ("-unchecked", "-feature", "-deprecation", "-optimize" )
