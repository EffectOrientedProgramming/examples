package Chapter10_Appendix_RunningEffects

import zio.*
import zio.direct.*
import zio.test.*

object Test0 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("random is random"):
      defer:
        ZIO.debug("** logic **").run
        assertTrue:
          10 > 2
  // ** logic **
  // Log: Signup initiated for Morty
  // Log: Signup initiated for Morty
  // + random is random
