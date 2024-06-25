package Chapter08_Reliability

import zio.*
import zio.direct.*
import zio.test.*

object Test0 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("long testZ"):
      defer:
        ZIO.sleep(1.hour).run
        assertCompletes
    @@ TestAspect.withLiveClock @@
      TestAspect.timeout(1.second)
  // - long testZ
  // Timeout of 1 s exceeded.


object Test1 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("long test!"):
      defer:
        spottyLogic.run
        assertCompletes
    @@ TestAspect.withLiveRandom @@
      TestAspect.flaky
  // Failed!
  // Failed!
  // Failed!
  // Failed!
  // Failed!
  // Failed!
  // Success!
  // + long test!
