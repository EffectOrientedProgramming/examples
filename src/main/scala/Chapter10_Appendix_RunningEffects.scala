import zio.*
import zio.direct.*

object HelloWorld extends zio.ZIOAppDefault:
  def run =
    ZIO.debug:
      "hello, world"

object Chapter10_Appendix_RunningEffects_0 extends ZIOAppDefault:
  def run =
    ZIO.debug:
      "hello, world"
  // hello, world
  // Result: ()


// NOTE We cannot execute invoke main on this
// because it crashes mdoc in the CI process
object RunningZIOs extends ZIOAppDefault:
  def run =
    Console.printLine:
      "Hello World!"

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