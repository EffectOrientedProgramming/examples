package Chapter08_Reliability

import zio.*
import zio.direct.*
import zio.test.*

object Test0 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("long test"):
      defer:
        spottyLogic.run
        assertCompletes
    @@ TestAspect.withLiveRandom @@ 
       TestAspect.flaky
  // Failed!
  // Failed!
  // Failed!
  // Failed!
  // Success!
  // + long test
  // Result: Summary(1,0,0,,PT0.021863S)
