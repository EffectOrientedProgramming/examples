package Chapter08_Reliability

import zio.*
import zio.direct.*
import zio.test.*

object Test0 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("long test"):
      defer:
        ZIO.sleep(1.hour).run
        assertCompletes
    @@ TestAspect.withLiveClock @@ 
       TestAspect.timeout(1.second)
  // - long test
  // Timeout of 1 s exceeded.
  // Result: 
  // - long test
  // Timeout of 1 s ex


object Test1 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("long test"):
      defer:
        // TODO More predictably random, 
        //   eg make sure it fails _at least_ x 
        //   times before succeeding
        spottyLogic.run
        assertCompletes
    @@ TestAspect.withLiveRandom @@ 
       TestAspect.flaky
  // Failed!
  // Success!
  // + long test
  // Result: Summary(1,0,0,,PT0.017732S)
