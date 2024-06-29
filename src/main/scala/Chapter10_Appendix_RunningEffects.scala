package Chapter10_Appendix_RunningEffects

import zio.*
import zio.direct.*

object HelloWorld extends zio.ZIOAppDefault:
  def run =
    ZIO.debug:
      "hello, world"

object App0 extends helpers.ZIOAppDebug:
  def run =
    ZIO.debug:
      "hello, world"
  // hello, world


// NOTE We cannot invoke main on this
// because it crashes mdoc in the CI process
object RunningZIOs extends ZIOAppDefault:
  def run =
    Console.printLine:
      "Hello World!"

// TODO Either find a better home for this, or delete.

val logic =
  defer:
    val username =
      Console
        .readLine:
          "Enter your name\n"
        .run
    Console
      .printLine:
        s"Hello $username"
      .run
  .orDie

val out =
  Unsafe.unsafe:
    implicit u: Unsafe =>
      Runtime
        .default
        .unsafe
        .run:
          ZIO.debug:
            "hello, world"
        .getOrThrowFiberFailure()