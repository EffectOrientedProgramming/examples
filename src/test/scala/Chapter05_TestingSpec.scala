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


object Test1 extends ZIOSpecDefault:
  def spec =
    test("Only the last assertTrue matters"):
      assertTrue(1 != 1) // Ignored
      assertTrue(1 == 1)
  // + Only the last assertTrue matters


object Test2 extends ZIOSpecDefault:
  def spec =
    test("Multiple Boolean expressions"):
      assertTrue(1 == 1, 2 == 2, 3 == 3)
  // + Multiple Boolean expressions


object Test3 extends ZIOSpecDefault:
  def spec =
    test("Combine using operators"):
      assertTrue(1 == 1) ||
      assertTrue(2 == 2) &&
      !assertTrue(42 == 47)
  // + Combine using operators


object Test4 extends ZIOSpecDefault:
  def spec =
    test("Effect as test"):
      defer:
        printLine("Effect in a test").run
        assertCompletes
  // Effect in a test
  // + Effect as test


def showLabel(label: String) =
  defer:
    printLine(s"Running $label").run
    assertCompletes

val effectA = showLabel("A")

object Test7 extends ZIOSpecDefault:
  def spec =
    test("case A"):
      effectA
  // Running A
  // + case A


object Test8 extends ZIOSpecDefault:
  val effectB = showLabel("B")
  
  def spec =
    suite("Suite of Tests")(
      test("case A in Suite"):
        effectA
      ,
      test("case A in Suite"):
        effectB,
    )
  // Running A
  // Running B
  // + Suite of Tests
  //   + case A in Suite
  //   + case A in Suite


import zio.test.test

def testCase(label: String) =
  test(s"case $label in a value"):
    showLabel(label)

val testA = testCase("A")
val testB = testCase("B")

object Test11 extends ZIOSpecDefault:
  def spec =
    suite("A Suite of Tests")(testA, testB)
  // Running A
  // Running B
  // + A Suite of Tests
  //   + case A in a value
  //   + case B in a value


class Material(val brittleness: Int)

object Material:
  val wood =
    ZLayer.succeed(Material(brittleness = 5))
  val plastic =
    ZLayer
      .succeed(Material(brittleness = 10))

class Saw(val intensity: Int)

object Saw:
  val hand =
    ZLayer.succeed(Saw(intensity = 4))
  val robotic =
    ZLayer.succeed(Saw(intensity = 8))

class Nailer(val intensity: Int)

object Nailer:
  val hand =
    ZLayer.succeed(Nailer(intensity = 4))
  val robotic =
    ZLayer.succeed(Nailer(intensity = 11))

val testToolWithMaterial =
  defer:
    val material = ZIO.service[Material].run
    val saw      = ZIO.service[Saw].run
    val nailer   = ZIO.service[Nailer].run
    assertTrue(
      saw.intensity < material.brittleness,
      nailer.intensity < material.brittleness,
    )

object Test15 extends ZIOSpecDefault:
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
  //     nailer = repl.MdocSession$MdocApp$Nailer@4f138263
  //     at <input>:277 
  // 
  //   + Plastic with robo saw & hammer


object Test16 extends ZIOSpecDefault:
  def spec =
    test("Capture output"):
      defer:
        printLine("Morty").run
        printLine("Beth").run
        val out2 = TestConsole.output.run
        assertTrue:
          out2 == Vector("Morty\n", "Beth\n")
  // Morty
  // Beth
  // + Capture output


object Test17 extends ZIOSpecDefault:
  val spec =
    test("Substitute input"):
      defer:
        TestConsole
          .feedLines("Morty", "Beth")
          .run
        val input = readLine.run
        assertTrue:
          input == "Morty"
  // + Substitute input


object Test18 extends ZIOSpecDefault:
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

object Test20 extends ZIOSpecDefault:
  def spec =
    test("batch runs after 24 hours"):
      defer:
        nightlyBatch.zipPar(timeTravel).run
        assertCompletes
  // Parsing CSV: ()
  // + batch runs after 24 hours
