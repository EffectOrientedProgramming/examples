import zio.*
import zio.direct.*
import zio.test.*

object Example04_Configuration_0 extends ZIOSpecDefault:
  def spec =
    test("flips 10 times"):
      defer:
        TestRandom
          .feedBooleans(true)
          .repeatN(9)
          .run
        assertTrue:
          flipTen.run == 10
  // Result: Test PASSED


object Example04_Configuration_1 extends ZIOSpecDefault:
  def spec =
    test("rosencrantzAndGuildensternAreDead finishes"):
      defer:
        TestRandom
          .feedBooleans:
            true
          .repeatN:
            7
          .run
        rosencrantzAndGuildensternAreDead.run
        assertCompletes
  // Result: Test PASSED


object Example04_Configuration_2 extends ZIOSpecDefault:
  def spec =
    test("flaky plan"):
      defer:
        rosencrantzAndGuildensternAreDead.run
        assertCompletes
    @@ TestAspect.withLiveRandom
    @@ TestAspect.flaky(Int.MaxValue)
  // Result: Test PASSED


object Example04_Configuration_3 extends ZIOSpecDefault:
  def spec =
    test("batch runs after 24 hours"):
      val timeTravel =
        TestClock.adjust:
          24.hours
    
      defer:
        nightlyBatch
          .race:
            timeTravel
          .run
    
        assertCompletes
  // Result: Test PASSED
