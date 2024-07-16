package Chapter05_Testing

import zio.*
import zio.direct.*
import zio.Console.*
import zio.test.*

object Test0 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    zio
      .test
      .test("basic"):
        assertTrue(1 == 1)
  // + basic


object Test1 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    zio
      .test
      .test("basic2"):
        assertTrue(1 != 1) // Ignored
        assertTrue(1 == 1)
  // + basic2


object Test2 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    zio
      .test
      .test("basic3"):
        // Multiple boolean expressions:
        assertTrue(1 == 1, 2 == 2, 3 == 3)
  // + basic3


object Test3 extends ZIOSpecDefault:
  import zio.test.*
  def spec =
    zio
      .test
      .test("basic4"):
        defer:
          printLine("testing basic4").run
          assertCompletes
  // testing basic4
  // + basic4


object Test4 extends ZIOSpecDefault:
  import zio.test.*
  val basic5 =
    defer:
      printLine("testing basic5").run
      assertCompletes
  
  def spec =
    zio
      .test
      .test("basic5"):
        basic5
  // testing basic5
  // + basic5


object Test5 extends ZIOSpecDefault:
  import zio.test.*
  val basic6 =
    defer:
      printLine("testing basic6").run
      assertTrue(1 == 1)
  
  def spec =
    suite("Creating a Suite of Tests")(
      zio
        .test
        .test("basic5 in suite"):
          basic6 // Duplicate to get it working
      ,
      zio
        .test
        .test("basic6 in suite"):
          basic6,
    )
  // testing basic6
  // testing basic6
  // + Creating a Suite of Tests
  //   + basic5 in suite
  //   + basic6 in suite


object Test6 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    zio
      .test
      .test("eat Bread"):
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


object Test7 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    zio
      .test
      .test("flips 10 times"):
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
