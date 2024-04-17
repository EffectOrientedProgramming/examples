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
    // spec190: ToTest[Nothing, Nothing] = mdoctools.ToTest@1e5975db
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
    // + flips 10 times
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
    // spec194: ToTest[String, Nothing] = mdoctools.ToTest@190f1d4a
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
    // + rosencrantzAndGuildensternAreDead finishes
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
    // spec229: ToTest[Nothing, Nothing] = mdoctools.ToTest@73707d4
    // Parsing CSV: ()
    // + batch runs after 24 hours
    // Result: Test PASSED
