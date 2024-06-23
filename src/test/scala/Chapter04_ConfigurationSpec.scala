package Chapter04_Configuration

import zio.*
import zio.direct.*
import zio.test.*

object Test0 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("eat Bread"):
      defer:
        ZIO
          .serviceWithZIO[Bread]:
            bread => 
              bread.eat
        .run
        val output = TestConsole.output.run
        assertTrue(output.contains("Bread: Eating\n"))
        
    .provide:
      IdealFriend
        .bread
  // Bread: Eating
  // + eat Bread
  // Result: Summary(1,0,0,,PT0.063907S)


object Test1 extends ZIOSpecDefault:
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
  // Result: Summary(1,0,0,,PT0.057273S)


object Test2 extends ZIOSpecDefault:
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
  // Result: Summary(1,0,0,,PT0.041565S)


object Test3 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("flaky plan"):
      defer:
        rosencrantzAndGuildensternAreDead.run
        assertCompletes
    @@ TestAspect.withLiveRandom @@
      TestAspect.flaky(Int.MaxValue)
  // *Performance Begins*
  // Heads
  // R: Heads
  // Heads
  // R: Heads
  // Tails
  // ...
  // R: Heads
  // G: ...probability
  // Heads
  // R: Heads
  // + flaky plan
  // Result: Summary(1,0,0,,PT0.038855S)


object Test4 extends ZIOSpecDefault:
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
  // Result: Summary(1,0,0,,PT0.039479S)
