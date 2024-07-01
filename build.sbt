ThisBuild / scalaVersion := "3.4.2"

val zioVersion = "2.1.5"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-direct" % "1.0.0-RC7",
  "dev.zio" %% "zio-config" % "4.0.2",
  "dev.zio" %% "zio-config-magnolia" % "4.0.2",
  "dev.zio" %% "zio-config-typesafe" % "4.0.2",
  "dev.zio" %% "zio-cache" % "0.2.3",
  "nl.vroste" %% "rezilience" % "0.9.4",
  "dev.zio" %% "zio-test" % zioVersion % Test,
  "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
)

fork := true

// running the examples requires Java 11 or newer
initialize := {
  initialize.value
  val required = VersionNumber("11")
  val current = VersionNumber(sys.props("java.specification.version"))
  assert(current.get(0).get >= required.get(0).get, s"Java $required or above required")
}

lazy val runMainClassesDoesNotTolerateFailures = taskKey[Unit]("Print main classes")
runMainClassesDoesNotTolerateFailures := Def.taskDyn {
  val s: TaskStreams = streams.value
  val classes = (Compile/discoveredMainClasses).value
  val tasks = classes.map{className =>
    Def.task {
      try {
        s.log.info(s"Running $className")
        (Compile/runMain).toTask(" " + className).value
      } catch {
        case e: Exception =>
          s.log.error(s"Failure when running $className: ${e.getMessage}")
      }
    }
  }
  Def.sequential(tasks)
}.value

// We need this version because many of the examples are expected to fail
lazy val runMainClassesToleratesFailures = taskKey[Unit]("Run all main classes")

runMainClassesToleratesFailures := {
  val log = streams.value.log
  val discovered: Seq[String] = (Compile/discoveredMainClasses).value
  // Get the classpath
  val classpath = Attributed.data((Compile/fullClasspath).value).mkString(":")

  discovered.foreach { className =>
    log.info(s"Running $className")
    val result = Fork.java.fork(ForkOptions(), Seq("-cp", classpath, className)).exitValue()
    if (result != 0) {
      log.error(s"Class $className failed with exit code $result")
    } else {
      log.info(s"Class $className completed successfully")
    }
  }
}