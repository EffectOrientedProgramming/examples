package Chapter09_Resilience

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
  import zio.Console._
  
  def spec =
    test("flaky test!"):
      defer:
        spottyLogic.run
        printLine("Continuing...").run
        assertCompletes
    @@ TestAspect.flaky
  // Failed!
  // Failed!
  // Failed!
  // Success!
  // Continuing...
  // + flaky test!
