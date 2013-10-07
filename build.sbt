name := "Lanyard"

version := "0.01"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq (
		    "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
		    "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
)

scalacOptions ++= Seq ("-unchecked", "-feature", "-deprecation")
