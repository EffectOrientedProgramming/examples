import zio.*
import zio.direct.*
import zio.test.*

object Chapter04_Configuration_0 extends ZIOSpecDefault:
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
  // + flips 10 times
  // Result: Summary(1,0,0,,PT0.055423S)


object Chapter04_Configuration_1 extends ZIOSpecDefault:
  def spec =
    test(
      "rosencrantzAndGuildensternAreDead finishes"
    ):
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
  // + rosencrantzAndGuildensternAreDead finishes
  // Result: Summary(1,0,0,,PT0.063351S)


object Chapter04_Configuration_2 extends ZIOSpecDefault:
  def spec =
    test("flaky plan"):
      defer:
        rosencrantzAndGuildensternAreDead.run
        assertCompletes
    @@ TestAspect.withLiveRandom @@
      TestAspect.flaky(Int.MaxValue)
  // *Performance Begins*
  // R: Heads
  // R: Heads
  // <FAIL> R: Fail(Tails,Stack trace for thread "zio-fiber-1557652809":
  // 	at repl.MdocSession.MdocApp.coinToss(<input>:400)
  // 	at repl.MdocSession.MdocApp.rosencrantzCoinToss(<input>:462)
  // ...
  // G: Though it can be done by luck alone.
  // R: Heads
  // G: ...probability
  // R: Heads
  // + flaky plan
  // Result: Summary(1,0,0,,PT0.027538S)


object Chapter04_Configuration_3 extends ZIOSpecDefault:
  def spec =
    test("batch runs after 24 hours"):
      val timeTravel =
        TestClock.adjust:
          24.hours
  
      defer:
        val fork =
          nightlyBatch.fork.run
        timeTravel.run
        fork.join.run
  
        assertCompletes
  // Parsing CSV: ()
  // + batch runs after 24 hours
  // Result: Summary(1,0,0,,PT0.024657S)
