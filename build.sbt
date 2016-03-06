name := "Rover"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.4"
libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.1.4"
libraryDependencies += "org.scalactic" %% "scalactic" % "2.2.6"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
