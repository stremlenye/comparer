name := """comparer"""

organization := "com.github.stremlenye"

version := "1.2.0-SNAPSHOT"

scalaVersion := "2.11.7"

licenses := Seq("MIT" → url("https://github.com/stremlenye/comparer/blob/master/LICENSE"))
homepage := Some(url("https://github.com/stremlenye/comparer"))
scalacOptions in Test ++= Seq("-Yrangepos")

// Change this to another test framework if you prefer
libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.7" % "test"
)

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomExtra := <url>http://jsuereth.com/scala-arm</url>
  <scm>
    <url>git@github.com:stremlenye/comparer.git</url>
    <connection>scm:git@github.com:stremlenye/comparer.git</connection>
  </scm>
  <developers>
    <developer>
      <id>stremlenye</id>
      <name>Yuriy Ankudinov</name>
      <url>https://github.com/stremlenye</url>
    </developer>
  </developers>
