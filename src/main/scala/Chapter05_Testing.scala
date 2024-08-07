package Chapter05_Testing

import zio.*
import zio.direct.*
import zio.Console.*

val coinToss =
  defer:
    if Random.nextBoolean.run then
      printLine("Heads").run
      ZIO.succeed("Heads").run
    else
      printLine("Tails").run
      ZIO.fail("Tails").run

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
  // Tails
  // Heads
  // Tails
  // Heads
  // Heads
  // Num Heads = 3
  // Result: 3


val nightlyBatch =
  ZIO.sleep(24.hours).debug("Parsing CSV")