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
        // this tries to create consistency for how we run and print output between the book and the examples
        ZIO.scoped:
          self.run
        .tapSome:
          case result if !result.isInstanceOf[Unit] =>
            Console.printLine:
              s"Result: $result"

    app.main(args)
