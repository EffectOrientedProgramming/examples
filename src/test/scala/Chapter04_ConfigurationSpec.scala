package Chapter04_Configuration

import zio.*
import zio.direct.*
import zio.test.*

object Test0 extends ZIOSpecDefault:
  import zio.test.*
  
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
  // Num Heads = 10
  // + flips 10 times
  // Result: Summary(1,0,0,,PT0.067352S)


object Test1 extends ZIOSpecDefault:
  import zio.test.*
  
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
  // Heads
  // R: Heads
  // Heads
  // R: Heads
  // Heads
  // ...
  // R: Heads
  // G: ...probability
  // Heads
  // R: Heads
  // + rosencrantzAndGuildensternAreDead finishes
  // Result: Summary(1,0,0,,PT0.052413S)


object Test2 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("flaky plan"):
      defer:
        rosencrantzAndGuildensternAreDead.run
        assertCompletes
    @@ TestAspect.withLiveRandom @@
      TestAspect.flaky(Int.MaxValue)
  // *Performance Begins*
  // Tails
  // <FAIL> R: Fail(Tails,Stack trace for thread "zio-fiber-1663268935":
  // 	at repl.MdocSession.MdocApp.coinToss(<input>:413)
  // 	at repl.MdocSession.MdocApp.rosencrantzCoinToss(<input>:482)
  // 	at repl.MdocSession.MdocApp.rosencrantzAndGuildensternAreDead(<input>:487)
  // ...
  // R: Heads
  // G: ...probability
  // Heads
  // R: Heads
  // + flaky plan
  // Result: Summary(1,0,0,,PT0.032433S)


object Test3 extends ZIOSpecDefault:
  import zio.test.*
  
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
  // Result: Summary(1,0,0,,PT0.022439S)
