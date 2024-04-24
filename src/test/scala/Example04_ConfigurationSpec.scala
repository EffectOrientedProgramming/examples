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
    // spec190: ToTest[Nothing, Nothing] = mdoctools.ToTest@291f204b
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
    // spec194: ToTest[String, Nothing] = mdoctools.ToTest@33deeb18
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
    + test("flaky plan"):
      defer:
        rosencrantzAndGuildensternAreDead.run
        assertCompletes
    @@ TestAspect.withLiveRandom
    @@ TestAspect.flaky(500)
    // spec197: ToTest[String, Nothing] = mdoctools.ToTest@5c909d4d
    // *Performance Begins*
    // R: Heads
    // R: Heads
    // R: Tails
    // *Performance Begins*
    // ...
    //   	at repl.MdocSession.MdocApp.debugDemo(<input>:404)
    //   	at repl.MdocSession.MdocApp.rosencrantzAndGuildensternAreDead(<input>:482)
    //   	at zio.direct.ZioMonad.Success.$anon.flatMap(ZioMonad.scala:19)
    //   	at zio.direct.ZioMonad.Success.$anon.map(ZioMonad.scala:18)
    //   	at repl.MdocSession.MdocApp.spec197(<input>:536)
    // Result: Test FAILED
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
    // spec213: ToTest[Nothing, Nothing] = mdoctools.ToTest@264f5eda
    // Parsing CSV: ()
    // + batch runs after 24 hours
    // Result: Test PASSED
