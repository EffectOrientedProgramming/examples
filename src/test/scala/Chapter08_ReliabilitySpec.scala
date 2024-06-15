package Chapter08_Reliability

import zio.*
import zio.direct.*
import zio.test.*

object Test0 extends ZIOSpecDefault:
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
  // Failed!
  // Success!
  // + long test
  // Result: Summary(1,0,0,,PT0.01792S)
