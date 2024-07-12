package Chapter05_Testing

import zio.*
import zio.direct.*

import zio.Console.*

trait Bread:
  def eat =
    printLine("Bread: Eating")

case class BreadFromFriend() extends Bread

object IdealFriend:
  val bread =
    ZLayer.succeed:
      BreadFromFriend()

val coinToss =
  defer:
    if Random.nextBoolean.run then
      ZIO.debug("Heads").run
      ZIO.succeed("Heads").run
    else
      ZIO.debug("Tails").run
      ZIO.fail("Tails").run

val flipTen =
  defer:
    val numHeads =
      ZIO
        .collectAllSuccesses:
          List.fill(10):
            coinToss
        .run
        .size
    ZIO.debug(s"Num Heads = $numHeads").run
    numHeads

object App0 extends helpers.ZIOAppDebug:
  def run =
    flipTen
  // Tails
  // Heads
  // Tails
  // Heads
  // Heads
  // Tails
  // Tails
  // Heads
  // Heads
  // Heads
  // Num Heads = 6
  // Result: 6


val nightlyBatch =
  ZIO
    .sleep:
      24.hours
    .debug:
      "Parsing CSV"