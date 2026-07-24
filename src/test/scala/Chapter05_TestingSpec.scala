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
      assertTrue(1 == 0) ||
      assertTrue(2 == 2) &&
      assertTrue(3 == 3)
  // + Combine using operators


object Test4 extends ZIOSpecDefault:
  def spec =
    test("negation"):
      !assertTrue(42 == 47)
  // + negation


object Test5 extends ZIOSpecDefault:
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

object Test8 extends ZIOSpecDefault:
  def spec =
    test("Case A"):
      effectA
  // Running A
  // + Case A


object Test9 extends ZIOSpecDefault:
  val effectB = showLabel("B")
  
  def spec =
    suite("Suite of Tests")(
      test("Case A in suite")(effectA),
      test("Case B in suite")(effectB),
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

object Test12 extends ZIOSpecDefault:
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

case class Nailer(force: Int)

object Nailer:
  val hand =
    ZLayer.succeed:
      Nailer(force = 4)

  val robotic =
    ZLayer.succeed:
      Nailer(force = 11)

val testNailerWithMaterial =
  defer:
    val material = ZIO.service[Material].run
    val nailer   = ZIO.service[Nailer].run
    assertTrue(
      nailer.force < material.brittleness
    )

object Test16 extends ZIOSpecDefault:
  def spec =
    suite("Construction Combinations")(
      test("Wood with hand nailer"):
        testNailerWithMaterial
          .provide(Material.wood, Nailer.hand)
      ,
      test("Plastic with hand nailer"):
        testNailerWithMaterial.provide(
          Material.plastic,
          Nailer.hand,
        )
      ,
      test("Plastic with robo nailer"):
        testNailerWithMaterial.provide(
          Material.plastic,
          Nailer.robotic,
        ),
    )
  // + Construction Combinations
  //   + Wood with hand nailer
  //   + Plastic with hand nailer
  //   - Plastic with robo nailer
  //     ✗ 11 was not less than 10
  //     nailer.force < material.brittleness
  //     .force = 11
  //     nailer = Nailer(force = 11)
  //


object Test17 extends ZIOSpecDefault:
  def spec =
    test("Flip 5 times"):
      defer:
        TestRandom
          .feedBooleans(true, true, true)
          .run
        TestRandom
          .feedBooleans(false, false)
          .run
        val heads =
          flipFive.debug("Number of Heads").run
        assertTrue(heads == 3)
  // Tails
  // Tails
  // Heads
  // Heads
  // Heads
  // Number of Heads: 3
  // + Flip 5 times


val timeTravel =
  TestClock.adjust:
    24.hours

object Test19 extends ZIOSpecDefault:
  def spec =
    test("Batch runs after 24 hours"):
      defer:
        nightlyBatch.zipPar(timeTravel).run
        assertCompletes
  // Parsing CSV: ()
  // + Batch runs after 24 hours
