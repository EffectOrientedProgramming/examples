package Chapter05_Testing

import zio.*
import zio.direct.*
import zio.Console.*
import zio.test.*

object Test0 extends ZIOSpecDefault:
  import zio.test.{test, assertTrue}
  
  def spec =
    test("basic"):
      assertTrue(1 == 1)
  // + basic


object Test1 extends ZIOSpecDefault:
  import zio.test.{test, assertTrue}
  
  def spec =
    test("basic2"):
      assertTrue(1 != 1) // Ignored
      assertTrue(1 == 1)
  // + basic2


object Test2 extends ZIOSpecDefault:
  import zio.test.{test, assertTrue}
  
  def spec =
    test("basic3"):
      // Multiple boolean expressions:
      assertTrue(1 == 1, 2 == 2, 3 == 3)
  // + basic3


object Test3 extends ZIOSpecDefault:
  import zio.test.{test, assertCompletes}
  
  def spec =
    test("basic4"):
      defer:
        printLine("testing basic4").run
        assertCompletes
  // testing basic4
  // + basic4


import zio.test.assertCompletes

val basic5 =
  defer:
    printLine("testing basic5").run
    assertCompletes

object Test5 extends ZIOSpecDefault:
  import zio.test.test
  
  def spec =
    test("basic5"):
      basic5
  // testing basic5
  // + basic5


object Test6 extends ZIOSpecDefault:
  import zio.test.{test, suite, assertTrue}
  
  val basic6 =
    defer:
      printLine("testing basic6").run
      assertTrue(1 == 1)
  
  def spec =
    suite("Creating a Suite of Tests")(
      test("basic5 in suite"):
        basic5
      ,
      test("basic6 in suite"):
        basic6,
    )
  // testing basic5
  // testing basic6
  // + Creating a Suite of Tests
  //   + basic5 in suite
  //   + basic6 in suite


trait Material:
  val brittleness: Int

case class Wood() extends Material:
  val brittleness = 5

case class Plastic() extends Material:
  val brittleness = 10

object Material:
  val wood    = ZLayer.succeed(Wood())
  val plastic = ZLayer.succeed(Plastic())

object Test8 extends ZIOSpecDefault:
  import zio.test.{
    test,
    assertTrue,
    TestConsole,
  }
  
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


object Test9 extends ZIOSpecDefault:
  import zio.test.{
    test,
    assertTrue,
    TestRandom,
  }
  
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
