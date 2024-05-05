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
  // + flips 10 times
  // Result: Summary(1,0,0,,PT0.111172S)


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
  // + rosencrantzAndGuildensternAreDead finishes
  // Result: Summary(1,0,0,,PT0.038036S)


object Example04_Configuration_2 extends ZIOSpecDefault:
  def spec =
    test("flaky plan"):
      defer:
        rosencrantzAndGuildensternAreDead.run
        assertCompletes
    @@ TestAspect.withLiveRandom
    @@ TestAspect.flaky(Int.MaxValue)
  // *Performance Begins*
  // R: Heads
  // <FAIL> R: Fail(Tails,Stack trace for thread "zio-fiber-942":
  // 	at repl.MdocSession.MdocApp.coinToss(<input>:395)
  // 	at repl.MdocSession.MdocApp.rosencrantzCoinToss(<input>:457)
  // 	at repl.MdocSession.MdocApp.rosencrantzAndGuildensternAreDead(<input>:462)
  // ...
  // G: Though it can be done by luck alone.
  // R: Heads
  // G: ...probability
  // R: Heads
  // + flaky plan
  // Result: Summary(1,0,0,,PT0.024962S)


object Example04_Configuration_3 extends ZIOSpecDefault:
  def spec =
    test("batch runs after 24 hours"):
      val timeTravel =
        TestClock.adjust:
          24.hours
    
      defer:
        val fork = nightlyBatch.fork.run
        timeTravel.run
        fork.join.run
    
        assertCompletes
  // Parsing CSV: ()
  // + batch runs after 24 hours
  // Result: Summary(1,0,0,,PT0.022564S)
