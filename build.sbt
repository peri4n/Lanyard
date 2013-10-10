name := "Lanyard"

version := "0.01"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq (
		    "org.scalatest" %% "scalatest" % "1.9.2" % "test",
		    "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
)

scalacOptions ++= Seq ("-unchecked", "-feature", "-deprecation")
