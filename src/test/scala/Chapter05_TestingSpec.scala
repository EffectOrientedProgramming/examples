package Chapter05_Testing

import zio.*
import zio.direct.*
import zio.Console.*
import zio.test.*

object Test0 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("basic"):
      assertTrue(1 == 1)
  // + basic


object Test1 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("Only the last assertTrue matters"):
      assertTrue(1 != 1) // Ignored
      assertTrue(1 == 1)
  // + Only the last assertTrue matters


object Test2 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("Multiple Boolean expressions"):
      assertTrue(1 == 1, 2 == 2, 3 == 3)
  // + Multiple Boolean expressions


object Test3 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("Combine using operators"):
      assertTrue(1 == 1) ||
      assertTrue(2 == 2) &&
      !assertTrue(42 == 47)
  // + Combine using operators


object Test4 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("Effect as test"):
      defer:
        printLine("This Effect is a test").run
        assertCompletes
  // This Effect is a test
  // + Effect as test


import zio.test.*

val aTest =
  defer:
    printLine("This is aTest").run
    assertCompletes

object Test6 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("aTest"):
      aTest
  // This is aTest
  // + aTest


object Test7 extends ZIOSpecDefault:
  import zio.test.*
  
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
  val intensity: Int

trait Saw extends Tool
case class HandSaw() extends Saw:
  val intensity = 4
case class RoboSaw() extends Saw:
  val intensity = 8

object Saw:
  val hand    = ZLayer.succeed(HandSaw())
  val robotic = ZLayer.succeed(RoboSaw())

trait Nailer extends Tool
case class Hammer() extends Nailer:
  val intensity = 4
case class RoboNailer() extends Nailer:
  val intensity = 11

object Nailer:
  val hand    = ZLayer.succeed(Hammer())
  val robotic = ZLayer.succeed(RoboNailer())

import zio.test.*

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
  import zio.test.*
  
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
  //     at <input>:220 
  // 
  //   + Plastic with robo saw & hammer


object Test12 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("Capture output"):
      defer:
        printLine("Morty").run
        val out1 = TestConsole.output.run
        printLine("Beth").run
        val out2 = TestConsole.output.run
        printLine(s"$out1\n****\n$out2").run
        printLine(out2(1)).run
        assertCompletes
  // Morty
  // Beth
  // Vector(Morty
  // )
  // ****
  // Vector(Morty
  // , Beth
  // )
  // Beth
  // 
  // + Capture output


object Test13 extends ZIOSpecDefault:
  val spec =
    test("Substitute input"):
      defer:
        TestConsole
          .feedLines("Morty", "Beth")
          .run
        val input = readLine.run
        printLine(input).run
        val output = TestConsole.output.run
        printLine(output).run
        assertTrue(input == "Morty")
  // Morty
  // Vector(Morty
  // )
  // + Substitute input


object Test14 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("flips 5 times"):
      defer:
        TestRandom
          .feedBooleans(
            true, true, false, true, false,
          )
          .run
        val heads = flipFive.run
        assertTrue(heads == 3)
  // Heads
  // Heads
  // Tails
  // Heads
  // Tails
  // Num Heads = 3
  // + flips 5 times


val timeTravel =
  TestClock.adjust:
    24.hours

object Test16 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("batch runs after 24 hours"):
      defer:
        nightlyBatch.zipPar(timeTravel).run
        assertCompletes
  // Parsing CSV: ()
  // + batch runs after 24 hours
