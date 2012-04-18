
name := "scoopshot"

version := "0.0.1"

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-optimize")

mainClass in (Compile, packageBin) := Some("Scoopshot") 

seq(com.twitter.sbt.GitProject.gitSettings: _*)

seq(com.twitter.sbt.PackageDist.newSettings: _*)

