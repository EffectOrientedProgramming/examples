package Chapter09_Resilience

import zio.*
import zio.direct.*
import zio.test.*

object Test0 extends ZIOSpecDefault:
  def spec =
    test("long testZ"):
      defer:
        ZIO.sleep(1.hour).run
        assertCompletes
    @@ TestAspect.withLiveClock
      @@ TestAspect.timeout(1.second)
  // - long testZ
  // Timeout of 1 s exceeded.


import zio.test.{assertTrue, test}

val troublesomeTestCase =
  test("flaky test!"):
    defer:
      val wasSuccessful = spottyLogic.run
      assertTrue(wasSuccessful)

object Test2 extends ZIOSpecDefault:
  def spec =
    troublesomeTestCase
  // Failed!
  // - flaky test!
  //   ✗ Result was false
  //   wasSuccessful
  //


object Test3 extends ZIOSpecDefault:
  def spec =
    troublesomeTestCase @@ TestAspect.flaky
  // Failed!
  // Failed!
  // Success!
  // + flaky test!
