name := """comparer"""

version := "1.0"

scalaVersion := "2.11.7"

scalacOptions in Test ++= Seq("-Yrangepos")

// Change this to another test framework if you prefer
libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.7" % "test"
)
