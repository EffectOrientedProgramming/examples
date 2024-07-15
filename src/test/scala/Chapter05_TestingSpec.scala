package Chapter05_Testing

import zio.*
import zio.direct.*
import zio.Console.*
import zio.test.*

object Test0 extends ZIOSpecDefault:
  def spec =
    test("basic"):
      assertTrue(1 == 1)
  // + basic


import zio.test.assertTrue

val testLogic =
  defer:
    assertTrue(1 == 1)

import zio.test.test

val testCase =
  test("eat Bread"):
    testLogic

object Test3 extends ZIOSpecDefault:
  def spec =
    testCase
  // + eat Bread


object Test4 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("eat Bread"):
      defer:
        ZIO
          .serviceWithZIO[Bread]:
            bread => bread.eat
          .run
        val output = TestConsole.output.run
        assertTrue:
          output.contains("Bread: Eating\n")
    .provide:
      IdealFriend.bread
  // Bread: Eating
  // + eat Bread


object Test5 extends ZIOSpecDefault:
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


import zio.test.*

val timeTravel =
  TestClock.adjust:
    24.hours

object Test7 extends ZIOSpecDefault:
  def spec =
    test("batch runs after 24 hours"):
      defer:
        nightlyBatch.zipPar(timeTravel).run
        assertCompletes
  // Parsing CSV: ()
  // + batch runs after 24 hours
