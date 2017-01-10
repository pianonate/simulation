scalaVersion := "2.12.1"
scalaSource in Compile := baseDirectory.value / "src"
scalacOptions ++= Seq("-opt:_")
