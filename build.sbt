scalaVersion := "2.12.1"
scalacOptions ++= Seq("-opt:_", "-target:jvm-1.8")
javaOptions in run += "-Xmx2G"
javaOptions in run += "-Xms2G"

// testing
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

// command line
libraryDependencies += "org.rogach" %% "scallop" % "2.0.6"

// logging
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.7"

// fork in run := true
//connectInput in run := true
cancelable in Global := true



/*
* following requires you to add to project/plugins.sbt
* addSbtPlugin("org.xerial.sbt" % "sbt-pack" % "0.8.2")  // for sbt-0.13.x or higher
 */
//packAutoSettings
packSettings
packMain := Map("simulation" -> "Main")
packJvmOpts := Map("simulation" -> Seq("-Xmx4G -Xms4G"))

