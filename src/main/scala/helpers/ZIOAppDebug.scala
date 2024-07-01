package helpers

import zio.*


// Enables us to add a debug to print the output of the program
trait ZIOAppDebug:
  self =>

  type Environment = Any

  val bootstrap: ZLayer[ZIOAppArgs, Any, Any] = ZLayer.empty

  val environmentTag: EnvironmentTag[Any] = EnvironmentTag[Any]

  def run: ZIO[ZIOAppArgs & Scope, Any, Any]

  def main(args: Array[String]): Unit =
    val app = new ZIOAppDefault:
      override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] =
        self.bootstrap

      override def run: ZIO[ZIOAppArgs & Scope, Any, Any] =
        ZIO.scoped(self.run).debug("Result")

    app.main(args)
