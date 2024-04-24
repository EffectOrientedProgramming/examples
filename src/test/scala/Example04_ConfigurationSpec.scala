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
  // Heads
  // Heads
  // Heads
  // Heads
  // Heads
  // Heads
  // Heads
  // Heads
  // Heads
  // Heads
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
  // *Performance Begins*
  // R: Heads
  // R: Heads
  // R: Heads
  // R: Heads
  // R: Heads
  // G: There is an art to building suspense.
  // R: Heads
  // G: Though it can be done by luck alone.
  // R: Heads
  // G: ...probability
  // R: Heads
  // Result: Test PASSED


object Example04_Configuration_2 extends ZIOSpecDefault:
  def spec =
    test("flaky plan"):
      defer:
        rosencrantzAndGuildensternAreDead.run
        assertCompletes
    @@ TestAspect.withLiveRandom
    @@ TestAspect.flaky(Int.MaxValue)
  // *Performance Begins*
  // R: Tails
  // *Performance Begins*
  // R: Heads
  // R: Heads
  // R: Tails
  // ...
  // R: Heads
  // G: Though it can be done by luck alone.
  // R: Heads
  // G: ...probability
  // R: Heads
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
  // Parsing CSV: ()
  // Result: Test PASSED
