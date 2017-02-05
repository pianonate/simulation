scalaVersion := "2.12.1"
scalaSource in Compile := baseDirectory.value / "src"
scalacOptions ++= Seq("-opt:_", "-target:jvm-1.8")
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.rogach" %% "scallop" % "2.0.6"
libraryDependencies += "net.java.dev.jna" % "jna-platform" % "4.1.0"
//libraryDependencies += "com.github.cb372" %% "scalacache-ehcache" % "0.9.3"
// fork in run := true
//connectInput in run := true
cancelable in Global := true
javaOptions in run += "-Xmx2048M"
javaOptions in run += "-Xms2048M"

/*
* following requires you to add to project/plugins.sbt
* addSbtPlugin("org.xerial.sbt" % "sbt-pack" % "0.8.2")  // for sbt-0.13.x or higher
 */
packAutoSettings