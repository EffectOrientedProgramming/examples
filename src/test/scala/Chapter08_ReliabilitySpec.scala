package Chapter08_Reliability

import zio.*
import zio.direct.*
import zio.test.*

object Test0 extends ZIOSpecDefault:
  import zio.test.*
  
  // TODO: Watch for indeterminate output - test should fail
  def spec =
    test("long testZ"):
      defer:
        ZIO.sleep(1.hour).run
        assertCompletes
    @@ TestAspect.withLiveClock @@
      TestAspect.timeout(1.second)
  // + long testZ


object Test1 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("long test!"):
      defer:
        spottyLogic.run
        assertCompletes
    @@ TestAspect.flaky
  // Failed!
  // Failed!
  // Success!
  // + long test!
