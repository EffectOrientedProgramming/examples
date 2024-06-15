package Chapter04_Configuration

import zio.*
import zio.direct.*

case class Dough():
  val letRise =
    Console.printLine:
      "Dough is rising"

object Dough:
  val fresh =
    ZLayer.derive[Dough]
      .tap(_ => Console.printLine("Dough: Mixed"))

object App0 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .serviceWithZIO[Dough]:
        dough => dough.letRise
      .provide:
        Dough.fresh
  // Dough: Mixed
  // Dough is rising


case class Heat()

val oven =
  ZLayer.derive[Heat]
    .tap(_ => Console.printLine("Oven: Heated"))

trait Bread {
  def eat =
    Console.printLine("Bread: Eating")
}

case class BreadHomeMade(
    heat: Heat,
    dough: Dough
) extends Bread

object Bread:
  val homemade =
    ZLayer.derive[BreadHomeMade]
      .tap(_ => Console.printLine("BreadHomeMade: Baked"))

object App1 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .serviceWithZIO[Bread]:
        bread => bread.eat
      .provide(Bread.homemade, Dough.fresh, oven)
  // Oven: Heated
  // Dough: Mixed
  // BreadHomeMade: Baked
  // Bread: Eating


case class Toast(heat: Heat, bread: Bread):
  val eat =
    Console.printLine("Toast: Eating")

object Toast:
  val make =
    ZLayer.derive[Toast]
      .tap(_ => Console.printLine("Toast: Made"))

object App2 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .service[Toast]
      .provide(
        Toast.make,
        Bread.homemade,
        Dough.fresh,
        oven,
      )
  // Oven: Heated
  // Dough: Mixed
  // BreadHomeMade: Baked
  // Toast: Made
  // Result: Toast(Heat(),BreadHomeMade(Heat(),Dough()))


val toaster =
  ZLayer.derive[Heat]
   .tap(_ => Console.printLine("Toaster: Heated"))

object App3 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .service[Heat]
      .provide:
        toaster
  // Toaster: Heated
  // Result: Heat()


case class Toaster()
object Toaster:
  val layer =
    ZLayer.derive[Toaster]
      .tap(_ => Console.printLine("Toaster: Heating"))

case class ToastZ(heat: Toaster, bread: Bread):
  val eat =
    Console.printLine("Toast: Eating")

object ToastZ:
  val make =
    ZLayer.derive[ToastZ]
      .tap(_ => Console.printLine("ToastZ: Made"))

object App4 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .serviceWithZIO[ToastZ]:
        toast => toast.eat
      .provide(
        ToastZ.make,
        Toaster.layer,
        Bread.homemade, 
        Dough.fresh, 
        oven,
      )
  // Oven: Heated
  // Dough: Mixed
  // BreadHomeMade: Baked
  // Toaster: Heating
  // ToastZ: Made
  // Toast: Eating


case class BreadStoreBought() extends Bread

val buyBread =
  ZIO.succeed:
    BreadStoreBought()

val storeBought =
  ZLayer.fromZIO:
    buyBread
  .tap(_ => Console.printLine("BreadStoreBought: Bought"))

object App5 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .service[Bread]
      .provide:
        storeBought
  // BreadStoreBought: Bought
  // Result: BreadStoreBought()


case class BreadFromFriend() extends Bread()
object Friend:
  def forcedFailure(invocations: Int) =
    defer:
      Console
        .printLine(
          s"Attempt $invocations: Error(Friend Unreachable)"
        )
        .run
      ZIO
        .when(true)(
          ZIO.fail("Error(Friend Unreachable)")
        )
        .as(???)
        .run
      ZIO.succeed(BreadFromFriend()).run

  def bread(worksOnAttempt: Int) =
    var invocations =
      0
    ZLayer.fromZIO:
      invocations += 1
      if invocations < worksOnAttempt then
        forcedFailure(invocations)
      else if invocations == 1 then
        ZIO.succeed(BreadFromFriend())
      else
        Console
          .printLine(
            s"Attempt $invocations: Succeeded"
          )
          .orDie
          .as:
            BreadFromFriend()
end Friend

object App6 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .service[Bread]
      .provide:
        Friend.bread(worksOnAttempt =
          3
        )
  // Attempt 1: Error(Friend Unreachable)
  // Result: Error(Friend Unreachable)


object App7 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .service[Bread]
      .provide:
        Friend
          .bread(worksOnAttempt =
            3
          )
          .orElse:
            storeBought
  // Attempt 1: Error(Friend Unreachable)
  // BreadStoreBought: Bought
  // Result: BreadStoreBought()


def logicWithRetries(retries: Int) = 
  ZIO
    .serviceWithZIO[Bread]:
      bread => bread.eat
    .provide:
      Friend
        .bread(worksOnAttempt =
          3
        )
        .retry:
          Schedule.recurs:
            retries
  

object App8 extends helpers.ZIOAppDebug:
  def run =
    logicWithRetries(retries = 1)
  // Attempt 1: Error(Friend Unreachable)
  // Attempt 2: Error(Friend Unreachable)
  // Result: Error(Friend Unreachable)


object App9 extends helpers.ZIOAppDebug:
  def run =
    logicWithRetries(retries = 2)
  // Attempt 1: Error(Friend Unreachable)
  // Attempt 2: Error(Friend Unreachable)
  // Attempt 3: Succeeded
  // Bread: Eating


import zio.config.*

case class RetryConfig(times: Int)


import zio.config.magnolia.deriveConfig

val configDescriptor: Config[RetryConfig] =
  deriveConfig[RetryConfig]

import zio.config.typesafe.*

val configProvider =
  ConfigProvider.fromHoconString:
    "{ times: 2 }"

val config =
  ZLayer.fromZIO:
    read:
      configDescriptor.from:
        configProvider

object App10 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .serviceWithZIO[RetryConfig]:
        retryConfig =>
          logicWithRetries(
            retries = retryConfig.times
          )
      .provide:
        config
  // Attempt 1: Error(Friend Unreachable)
  // Attempt 2: Error(Friend Unreachable)
  // Attempt 3: Succeeded
  // Bread: Eating


val ovenSafe =
  ZLayer.fromZIO:
    ZIO.succeed(Heat())
      .tap(_ => Console.printLine("Oven: Heated"))
      .withFinalizer(_ => Console.printLine("Oven: Turning off!").orDie)

object App11 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .serviceWithZIO[Bread]:
        bread => bread.eat
      .provide(
        Bread.homemade, 
        Dough.fresh, 
        ovenSafe, 
        Scope.default
      )
  // Oven: Heated
  // Dough: Mixed
  // BreadHomeMade: Baked
  // Bread: Eating
  // Oven: Turning off!


val coinToss =
  defer:
    if Random.nextBoolean.run then
      ZIO.debug("Heads").run
      ZIO
        .succeed:
          "Heads"
        .run
    else
      ZIO.debug("Tails").run
      ZIO
        .fail:
          "Tails"
        .run

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

object App12 extends helpers.ZIOAppDebug:
  def run =
    flipTen
  // Heads
  // Tails
  // Heads
  // Heads
  // Heads
  // Tails
  // Tails
  // Tails
  // Heads
  // Heads
  // Num Heads = 6
  // Result: 6


val rosencrantzCoinToss =
  coinToss.debug:
    "R"

val rosencrantzAndGuildensternAreDead =
  defer:
    ZIO
      .debug:
        "*Performance Begins*"
      .run
    rosencrantzCoinToss.repeatN(4).run

    ZIO
      .debug:
        "G: There is an art to building suspense."
      .run
    rosencrantzCoinToss.run

    ZIO
      .debug:
        "G: Though it can be done by luck alone."
      .run
    rosencrantzCoinToss.run

    ZIO
      .debug:
        "G: ...probability"
      .run
    rosencrantzCoinToss.run

val nightlyBatch =
  ZIO
    .sleep:
      24.hours
    .debug:
      "Parsing CSV"