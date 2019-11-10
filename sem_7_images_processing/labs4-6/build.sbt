name := "labs4-6"

version := "0.1"

scalaVersion := "2.12.10"

lazy val common = project
lazy val lab4 = project.dependsOn(common)
//lazy val lab5 = project

ThisBuild / libraryDependencies += "com.sksamuel.scrimage" %% "scrimage-core" % "2.1.8"
ThisBuild / libraryDependencies += "org.scalanlp" %% "breeze" % "1.0"
ThisBuild / libraryDependencies += "org.scalanlp" %% "breeze-viz" % "1.0"