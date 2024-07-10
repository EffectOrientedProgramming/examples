package Chapter04_Initialization

import zio.*
import zio.direct.*

import zio.Console._

trait Bread:
  def eat =
    printLine("Bread: Eating")

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


import zio.Console.*
case class X():
  val display =
    printLine("X.f")

val makeX =
  defer:
    printLine("Creating X").run
    X()

val dependency =
  ZLayer.fromZIO:
    makeX

object X:
  val dependent =
    ZLayer.fromZIO:
      makeX

object App1 extends helpers.ZIOAppDebug:
  object NamingExampleX extends ZIOAppDefault:
    def run =
      ZIO
        .serviceWithZIO[X]:
          x => x.display
        .provide:
          X.dependent // The "adjectivized object"
        // dependency // Or the noun version


import zio.Console._

case class Dough():
  val letRise =
    printLine:
      "Dough: rising"

import zio.Console._

object Dough:
  val fresh = // TODO: should be a noun
    ZLayer.fromZIO:
      defer:
        printLine("Dough: Mixed").run
        Dough()

import zio.Console._

case class Heat()

val oven =
  ZLayer.fromZIO:
    defer:
      printLine("Oven: Heated").run
      Heat()

import zio.Console._

case class BreadHomeMade(
    heat: Heat,
    dough: Dough
) extends Bread

object Bread:
  val homemade = // TODO: should be a noun
    ZLayer.fromZIO:
      defer:
        printLine("BreadHomeMade: Baked").run
        BreadHomeMade(
          ZIO.service[Heat].run,
          ZIO.service[Dough].run
        )

object App2 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .serviceWithZIO[Bread]:
        bread => bread.eat
      .provide(
        Bread.homemade,
        Dough.fresh,
        oven
      )


import zio.Console._

case class Toast(heat: Heat, bread: Bread):
  val eat =
    printLine:
      "Toast: Eating"

object Toast:
  val make = // TODO: should be a noun
    ZLayer.fromZIO:
      defer:
        printLine("Toast: Made").run
        Toast(
          ZIO.service[Heat].run,
          ZIO.service[Bread].run
        )

object App3 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .service[Toast]
      .provide(
        Toast.make,
        Bread.homemade,
        Dough.fresh,
        oven
      )


import zio.Console._

val toaster =
  ZLayer.fromZIO:
    defer:
      printLine("Toaster: Heated").run
      Heat()

object App4 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .service[Heat]
      .provide:
        toaster


import zio.Console._

case class Toaster()

object Toaster:
  val layer =
    ZLayer.fromZIO:
      defer:
        printLine("Toaster: Heating").run
        Toaster()

import zio.Console._

case class ToastZ(
    heat: Toaster,
    bread: Bread
):
  val eat =
    printLine:
      "Toast: Eating"

object ToastZ:
  val make = // TODO: should be a noun
    ZLayer.fromZIO:
      defer:
        printLine("ToastZ: Made").run
        ToastZ(
          ZIO.service[Toaster].run,
          ZIO.service[Bread].run
        )

object App5 extends helpers.ZIOAppDebug:
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


import zio.Console._

val ovenSafe =
  ZLayer.fromZIO:
    ZIO
      .succeed(Heat())
      .tap:
        _ =>
          printLine:
            "Oven: Heated"
      .withFinalizer:
        _ =>
          printLine:
            "Oven: Turning off!"
          .orDie

object App6 extends helpers.ZIOAppDebug:
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


import zio.Console._

case class BreadFromFriend() extends Bread()
object Friend:
  def forcedFailure(invocations: Int) =
    defer:
      printLine(
        s"Attempt $invocations: Failure(Friend Unreachable)"
      ).run
      ZIO
        .when(true)(
          ZIO.fail(
            "Failure(Friend Unreachable)"
          )
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
        printLine(
          s"Attempt $invocations: Succeeded"
        ).orDie
          .as:
            BreadFromFriend()
end Friend

object App7 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .service[Bread]
      .provide:
        Friend.bread(worksOnAttempt =
          3
        )


object App8 extends helpers.ZIOAppDebug:
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

object App9 extends helpers.ZIOAppDebug:
  def run =
    logicWithRetries(retries =
      2
    )


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
          logicWithRetries(retries =
            retryConfig.times
          )
      .provide:
        config


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

object App11 extends helpers.ZIOAppDebug:
  def run =
    flipTen


val nightlyBatch =
  ZIO
    .sleep:
      24.hours
    .debug:
      "Parsing CSV"