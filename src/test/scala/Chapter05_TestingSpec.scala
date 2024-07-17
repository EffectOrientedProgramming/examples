package Chapter05_Testing

import zio.*
import zio.direct.*
import zio.Console.*
import zio.test.*

object Test0 extends ZIOSpecDefault:
  import zio.test.{test, *}
  
  def spec =
    test("basic"):
      assertTrue(1 == 1)
  // + basic


object Test1 extends ZIOSpecDefault:
  import zio.test.{test, *}
  
  def spec =
    test("Only the last assertTrue matters"):
      assertTrue(1 != 1) // Ignored
      assertTrue(1 == 1)
  // + Only the last assertTrue matters


object Test2 extends ZIOSpecDefault:
  import zio.test.{test, *}
  
  def spec =
    test("Multiple Boolean expressions"):
      assertTrue(1 == 1, 2 == 2, 3 == 3)
  // + Multiple Boolean expressions


object Test3 extends ZIOSpecDefault:
  import zio.test.{test, *}
  
  def spec =
    test("Combine using operators"):
      assertTrue(1 == 1) ||
      assertTrue(2 == 2) &&
      !assertTrue(42 == 47)
  // + Combine using operators


object Test4 extends ZIOSpecDefault:
  import zio.test.{test, *}
  
  def spec =
    test("Effect as test"):
      defer:
        printLine("This Effect is a test").run
        assertCompletes
  // This Effect is a test
  // + Effect as test


import zio.test.assertCompletes

val aTest =
  defer:
    printLine("This is aTest").run
    assertCompletes

object Test6 extends ZIOSpecDefault:
  import zio.test.test
  
  def spec =
    test("aTest"):
      aTest
  // This is aTest
  // + aTest


object Test7 extends ZIOSpecDefault:
  import zio.test.{test, *}
  
  val bTest =
    defer:
      printLine("This is bTest").run
      assertTrue(1 == 1)
  
  def spec =
    suite("A Suite of Tests")(
      test("aTest in Suite"):
        aTest
      ,
      test("bTest in Suite"):
        bTest,
    )
  // This is aTest
  // This is bTest
  // + A Suite of Tests
  //   + aTest in Suite
  //   + bTest in Suite


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

object Test11 extends ZIOSpecDefault:
  import zio.test.{test, *}
  
  def spec =
    suite("Materials with different Tools")(
      test("Wood with hand tools"):
        testToolWithMaterial.provide(
          Material.wood,
          Saw.hand,
          Nailer.hand,
        )
      ,
      test("Plastic with hand tools"):
        testToolWithMaterial.provide(
          Material.plastic,
          Saw.hand,
          Nailer.hand,
        )
      ,
      test("Plastic with robo tools"):
        testToolWithMaterial.provide(
          Material.plastic,
          Saw.robotic,
          Nailer.robotic,
        )
      ,
      test("Plastic with robo saw & hammer"):
        testToolWithMaterial.provide(
          Material.plastic,
          Saw.robotic,
          Nailer.hand,
        ),
    )
  // + Materials with different Tools
  //   + Wood with hand tools
  //   + Plastic with hand tools
  //   - Plastic with robo tools
  //     ✗ 11 was not less than 10
  //     nailer.intensity < material.brittleness,
  //     .intensity = 11
  //     nailer = RoboNailer()
  //     at <input>:227 
  // 
  //   + Plastic with robo saw & hammer


object Test12 extends ZIOSpecDefault:
  import zio.test.{test, *}
  
  def spec =
    test("flips 10 times"):
      defer:
        TestRandom
          .feedBooleans(true)
          .repeatN(9)
          .run
        val result = flipTen.run
        assertTrue(result == 10)
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
