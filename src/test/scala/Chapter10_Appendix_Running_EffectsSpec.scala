package Chapter10_Appendix_Running_Effects

import zio.*
import zio.direct.*
import zio.test.*

object Test0 extends ZIOSpecDefault:
  def spec =
    test("Hello Test"):
      defer:
        ZIO.debug("** logic **").run
        assertTrue(10 > 2)
  // ** logic **
  // + Hello Test
