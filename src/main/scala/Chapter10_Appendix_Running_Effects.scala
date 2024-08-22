package Chapter10_Appendix_Running_Effects

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