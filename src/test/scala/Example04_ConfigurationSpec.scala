import zio.*
import zio.direct.*
import zio.test.*

object Example04_ConfigurationSpec extends ZIOSpecDefault:
  def spec = suite("suite"):
    test("flips 10 times"):
      defer:
        TestRandom
          .feedBooleans(true)
          .repeatN(9)
          .run
        assertTrue:
          flipTen.run == 10
    // spec190: ToTest[Nothing, Nothing] = mdoctools.ToTest@1b12911f
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
    + test("rosencrantzAndGuildensternAreDead finishes"):
      defer:
        TestRandom
          .feedBooleans:
            true
          .repeatN:
            7
          .run
        rosencrantzAndGuildensternAreDead.run
        assertCompletes
    // spec194: ToTest[String, Nothing] = mdoctools.ToTest@615d500e
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
    + test("batch runs after 24 hours"):
      val timeTravel =
        TestClock.adjust:
          24.hours
    
      defer:
        nightlyBatch
          .race:
            timeTravel
          .run
    
        assertCompletes
    // spec229: ToTest[Nothing, Nothing] = mdoctools.ToTest@21b26eb6
    // Parsing CSV: ()
    // Result: Test PASSED
