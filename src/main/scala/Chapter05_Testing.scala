package Chapter05_Testing

import zio.*
import zio.direct.*
import zio.Console.*

import zio.{Console, *}
val coinToss =
  defer:
    if Random.nextBoolean.run then
      printLine("Heads").run
      ZIO.succeed("Heads").run
    else
      printLine("Tails").run
      ZIO.fail("Tails").run

import zio.{Console, *}
val flipFive =
  defer:
    val numHeads =
      ZIO
        .collectAllSuccesses:
          List.fill(5):
            coinToss
        .run
        .size
    printLine(s"Num Heads = $numHeads").run
    numHeads

object App0 extends helpers.ZIOAppDebug:
  def run =
    flipFive
  // Heads
  // Tails
  // Heads
  // Tails
  // Tails
  // Num Heads = 2
  // Result: 2


val nightlyBatch =
  ZIO.sleep(24.hours).debug("Parsing CSV")