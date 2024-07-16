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

trait Tool:
  val action: String
  val intensity: Int
  val use =
    printLine(
      s"$this $action, intensity $intensity"
    )

trait Saw extends Tool:
  val action = "sawing"
case class HandSaw() extends Saw:
  val intensity = 4
case class RoboSaw() extends Saw:
  val intensity = 8

object Saw:
  val hand    = ZLayer.succeed(HandSaw())
  val robotic = ZLayer.succeed(RoboSaw())

trait Nailer extends Tool:
  val action = "nailing"
case class Hammer() extends Nailer:
  val intensity = 4
case class RoboNailer() extends Nailer:
  val intensity = 11

object Nailer:
  val hand    = ZLayer.succeed(Hammer())
  val robotic = ZLayer.succeed(RoboNailer())

import zio.test.assertTrue

val testToolWithMaterial =
  defer:
    val material = ZIO.service[Material].run
    val saw      = ZIO.service[Saw].run
    val nailer   = ZIO.service[Nailer].run
    assertTrue(
      saw.intensity < material.brittleness,
      nailer.intensity < material.brittleness,
    )

object Test10 extends ZIOSpecDefault:
  import zio.test.{test, suite}
  
  def spec =
    suite("Materials with different Tools")(
      test("Wood with Hand tools"):
        testToolWithMaterial.provide(
          Material.wood,
          Saw.hand,
          Nailer.hand,
        )
      ,
      test("Plastic with Hand tools"):
        testToolWithMaterial.provide(
          Material.plastic,
          Saw.hand,
          Nailer.hand,
        )
      ,
      test("Plastic with Robo tools"):
        testToolWithMaterial.provide(
          Material.plastic,
          Saw.robotic,
          Nailer.robotic,
        )
      ,
      test("Plastic with Robo saw & hammer"):
        testToolWithMaterial.provide(
          Material.plastic,
          Saw.robotic,
          Nailer.hand,
        ),
    )
  // + Materials with different Tools
  //   + Wood with Hand tools
  //   + Plastic with Hand tools
  //   - Plastic with Robo tools
  //     ✗ 11 was not less than 10
  //     nailer.intensity < material.brittleness,
  //     .intensity = 11
  //     nailer = RoboNailer()
  //     at <input>:215 
  // 
  //   + Plastic with Robo saw & hammer


object Test11 extends ZIOSpecDefault:
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
