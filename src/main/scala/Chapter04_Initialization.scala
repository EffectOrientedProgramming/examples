package Chapter04_Initialization

import zio.*
import zio.direct.*

trait Bread:
  def eat =
    Console.printLine:
      "Bread: Eating"

case class BreadStoreBought() extends Bread

object BreadStoreBought:
  val layer =
    ZLayer.succeed:
      BreadStoreBought()

object App0 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .serviceWithZIO[Bread]:
        bread => bread.eat
      .provide:
        BreadStoreBought.layer
  // Bread: Eating


// todo: explain the letRise being an Effect, i.e. dependencies can themselves be Effects
case class Dough():
  val letRise =
    Console.printLine:
      "Dough is rising"

object Dough:
  val fresh =
    ZLayer.fromZIO:
      defer:
        Console.printLine("Dough: Mixed").run
        Dough()

case class Heat()

val oven =
  ZLayer.fromZIO:
    defer:
      Console.printLine("Oven: Heated").run
      Heat()

case class BreadHomeMade(
    heat: Heat,
    dough: Dough
) extends Bread

object Bread:
  val homemade =
    ZLayer.fromZIO:
      defer:
        Console
          .printLine("BreadHomeMade: Baked")
          .run
        BreadHomeMade(
          ZIO.service[Heat].run,
          ZIO.service[Dough].run
        )

object App1 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .serviceWithZIO[Bread]:
        bread => bread.eat
      .provide(
        Bread.homemade,
        Dough.fresh,
        oven
      )
  // Oven: Heated
  // Dough: Mixed
  // BreadHomeMade: Baked
  // Bread: Eating


case class Toast(heat: Heat, bread: Bread):
  val eat =
    Console.printLine:
      "Toast: Eating"

object Toast:
  val make =
    ZLayer.fromZIO:
      defer:
        Console.printLine("Toast: Made").run
        Toast(
          ZIO.service[Heat].run,
          ZIO.service[Bread].run
        )

object App2 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .service[Toast]
      .provide(
        Toast.make,
        Bread.homemade,
        Dough.fresh,
        oven
      )
  // Oven: Heated
  // Dough: Mixed
  // BreadHomeMade: Baked
  // Toast: Made
  // Result: Toast(Heat(),BreadHomeMade(Heat(),Dough()))


val toaster =
  ZLayer.fromZIO:
    defer:
      Console
        .printLine("Toaster: Heated")
        .run
      Heat()

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
    ZLayer.fromZIO:
      defer:
        Console
          .printLine("Toaster: Heating")
          .run
        Toaster()

case class ToastZ(
    heat: Toaster,
    bread: Bread
):
  val eat =
    Console.printLine:
      "Toast: Eating"

object ToastZ:
  val make =
    ZLayer.fromZIO:
      defer:
        Console.printLine("ToastZ: Made").run
        ToastZ(
          ZIO.service[Toaster].run,
          ZIO.service[Bread].run
        )

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
        oven
      )
  // Toaster: Heating
  // Oven: Heated
  // Dough: Mixed
  // BreadHomeMade: Baked
  // ToastZ: Made
  // Toast: Eating


val ovenSafe =
  ZLayer.fromZIO:
    ZIO
      .succeed(Heat())
      .tap:
        _ =>
          Console.printLine:
            "Oven: Heated"
      .withFinalizer:
        _ =>
          Console
            .printLine:
              "Oven: Turning off!"
            .orDie

object App5 extends helpers.ZIOAppDebug:
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


case class BreadFromFriend() extends Bread()
object Friend:
  def forcedFailure(invocations: Int) =
    defer:
      Console
        .printLine(
          s"Attempt $invocations: Failure(Friend Unreachable)"
        )
        .run
      ZIO
        .when(true)(
          ZIO.fail(
            "Failure(Friend Unreachable)"
          ) // TODO: Replace error with failure pervasively
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
  // Attempt 1: Failure(Friend Unreachable)
  // Result: Failure(Friend Unreachable)


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
            BreadStoreBought.layer
  // Attempt 1: Failure(Friend Unreachable)
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
    logicWithRetries(retries =
      2
    )
  // Attempt 1: Failure(Friend Unreachable)
  // Attempt 2: Failure(Friend Unreachable)
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

object App9 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .serviceWithZIO[RetryConfig]:
        retryConfig =>
          logicWithRetries(retries =
            retryConfig.times
          )
      .provide:
        config
  // Attempt 1: Failure(Friend Unreachable)
  // Attempt 2: Failure(Friend Unreachable)
  // Attempt 3: Succeeded
  // Bread: Eating


object IdealFriend:
  val bread =
    ZLayer.succeed:
      BreadFromFriend()

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

object App10 extends helpers.ZIOAppDebug:
  def run =
    flipTen
  // Heads
  // Tails
  // Heads
  // Tails
  // Heads
  // Tails
  // Tails
  // Heads
  // Heads
  // Tails
  // Num Heads = 5
  // Result: 5


val nightlyBatch =
  ZIO
    .sleep:
      24.hours
    .debug:
      "Parsing CSV"