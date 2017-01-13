scalaVersion := "2.12.1"
scalaSource in Compile := baseDirectory.value / "src"
scalacOptions ++= Seq("-opt:_")
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

