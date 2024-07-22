package Chapter10_Appendix_Running_Effects

import zio.*
import zio.direct.*
import zio.Console.*

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
    printLine:
      "Hello World!"

val out =
  Unsafe.unsafe:
    implicit u: Unsafe =>
      Runtime
        .default
        .unsafe
        .run:
          ZIO.debug:
            "hello, world"
        .getOrThrow()
// hello, world