package Chapter05_Testing

import zio.*
import zio.direct.*
import zio.test.*

object Test0 extends ZIOSpecDefault:
  def spec =
    test("Basic"):
      assertTrue(1 == 1)
  // + Basic


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
        ZIO.debug("Executing logic").run
        assertCompletes
  // Executing logic
  // + Effect as test


def showLabel(label: String) =
  defer:
    ZIO.debug(s"Running $label").run
    assertCompletes

val effectA = showLabel("A")

object Test7 extends ZIOSpecDefault:
  def spec =
    test("Case A"):
      effectA
  // Running A
  // + Case A


object Test8 extends ZIOSpecDefault:
  val effectB = showLabel("B")
  
  def spec =
    suite("Suite of Tests")(
      test("Case A in suite"):
        effectA
      ,
      test("Case B in suite"):
        effectB,
    )
  // Running A
  // Running B
  // + Suite of Tests
  //   + Case A in suite
  //   + Case B in suite


import zio.test.test

def testCase(label: String) =
  test(s"Case $label in a value"):
    showLabel(label)

val testA = testCase("A")
val testB = testCase("B")

object Test11 extends ZIOSpecDefault:
  def spec =
    suite("A Suite of Tests")(testA, testB)
  // Running A
  // Running B
  // + A Suite of Tests
  //   + Case A in a value
  //   + Case B in a value


case class Material(brittleness: Int)

object Material:
  val wood =
    ZLayer.succeed:
      Material(brittleness = 5)
  val plastic =
    ZLayer.succeed:
      Material(brittleness = 10)

case class Saw(force: Int)

object Saw:
  val hand =
    ZLayer.succeed:
      Saw(force = 4)

  val robotic =
    ZLayer.succeed:
      Saw(force = 8)

case class Nailer(force: Int)

object Nailer:
  val hand =
    ZLayer.succeed:
      Nailer(force = 4)

  val robotic =
    ZLayer.succeed:
      Nailer(force = 11)

val testToolWithMaterial =
  defer:
    val material = ZIO.service[Material].run
    val saw      = ZIO.service[Saw].run
    val nailer   = ZIO.service[Nailer].run
    assertTrue(
      saw.force < material.brittleness,
      nailer.force < material.brittleness,
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
  //     nailer.force < material.brittleness,
  //     .force = 11
  //     nailer = Nailer(force = 11)
  // 
  //   + Plastic with robo saw & hammer


object Test16 extends ZIOSpecDefault:
  def spec =
    test("Flip 5 times"):
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
  // + Flip 5 times


val timeTravel =
  TestClock.adjust:
    24.hours

object Test18 extends ZIOSpecDefault:
  def spec =
    test("Batch runs after 24 hours"):
      defer:
        nightlyBatch.zipPar(timeTravel).run
        assertCompletes
  // Parsing CSV: ()
  // + Batch runs after 24 hours
