package Chapter05_Testing

import zio.*
import zio.direct.*

val coinToss =
  defer:
    if Random.nextBoolean.run then
      ZIO.debug("Heads").run
      ZIO.succeed("Heads").run
    else
      ZIO.debug("Tails").run
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
    ZIO.debug(s"Num Heads = $numHeads").run
    numHeads

object App0 extends helpers.ZIOAppDebug:
  def run =
    flipFive
  // Heads
  // Tails
  // Heads
  // Heads
  // Heads
  // Num Heads = 4
  // Result: 4


val nightlyBatch =
  ZIO.sleep(24.hours).debug("Parsing CSV")