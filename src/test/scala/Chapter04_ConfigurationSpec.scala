package Chapter04_Configuration

import zio.*
import zio.direct.*
import zio.test.*

import zio.test.assertTrue

val logic =
  defer:
    assertTrue(1==1)

import zio.test.test

val testCase =
  test("eat Bread"):
    logic

object Test2 extends ZIOSpecDefault:
  def spec =
    testCase
  // TODO TestSummary renderer?
  // + eat Bread
  // Result: Summary(1,0,0,,PT0.044601S)


object Test3 extends ZIOSpecDefault:
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
  // Result: Summary(1,0,0,,PT0.047538S)


object Test4 extends ZIOSpecDefault:
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
  // Result: Summary(1,0,0,,PT0.03783S)


object Test5 extends ZIOSpecDefault:
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
  // Result: Summary(1,0,0,,PT0.065002S)
