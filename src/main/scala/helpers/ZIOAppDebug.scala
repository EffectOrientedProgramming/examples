package helpers

import zio.*


// Enables us to add a debug to print the output of the program
trait ZIOAppDebug extends App:
  self =>

    val bootstrap: ZLayer[ZIOAppArgs, Any, Any] = ZLayer.empty

    def run: ZIO[ZIOAppArgs & Scope, Any, Any]

    private val app = new ZIOAppDefault:
      override def run: ZIO[Any & ZIOAppArgs & Scope, Any, Any] =
        self.run.debug("Result")

    app.main(Array.empty)
