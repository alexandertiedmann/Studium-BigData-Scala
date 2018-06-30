name := "ConcurrencyAufgabe"

version := "0.1"

scalaVersion := "2.12.5"

libraryDependencies ++=Seq("junit" % "junit" % "4.11",
"org.scalactic" %% "scalactic" % "3.0.5",
			   "org.scalatest" %% "scalatest" % "3.0.5" % "test"	)

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.8.2"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution := false

