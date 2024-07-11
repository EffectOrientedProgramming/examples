package Chapter04_Initialization

import zio.*
import zio.direct.*
import zio.test.*

import zio.test.assertTrue

val logic =
  defer:
    assertTrue(1 == 1)

import zio.test.test

val testCase =
  test("eat Bread"):
    logic

object Test2 extends ZIOSpecDefault:
  def spec =
    testCase


object Test3 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("eat Bread"):
      defer:
        ZIO
          .serviceWithZIO[Bread]:
            bread => bread.eat
          .run
        val output =
          TestConsole.output.run
        assertTrue(
          output.contains("Bread: Eating\n")
        )
    .provide:
      IdealFriend.bread


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


import zio.test.*

val timeTravel =
  TestClock.adjust:
    24.hours

object Test6 extends ZIOSpecDefault:
  def spec =
    test("batch runs after 24 hours"):
      defer:
        nightlyBatch.zipPar(timeTravel).run
        assertCompletes
