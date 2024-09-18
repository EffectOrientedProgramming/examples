package Chapter04_Initialization

import zio.*
import zio.direct.*

trait Bread:
  val eat = ZIO.debug("Bread: Eating")

class BreadStoreBought extends Bread

val purchaseBread =
  defer:
    ZIO.debug("Buying bread").run
    BreadStoreBought()

val storeBoughtBread =
  ZLayer.fromZIO:
    purchaseBread

val eatBread =
  ZIO.serviceWithZIO[Bread]:
    bread => bread.eat

object App0 extends helpers.ZIOAppDebug:
  def run =
    eatBread.provide:
      storeBoughtBread
  // Buying bread
  // Bread: Eating


class Dough:
  val letRise = ZIO.debug("Dough: rising")

object Dough:
  val fresh =
    ZLayer.fromZIO:
      defer:
        ZIO.debug("Dough: Mixed").run
        Dough()

trait HeatSource

class Oven extends HeatSource

object Oven:
  val heated =
    ZLayer.fromZIO:
      defer:
        ZIO.debug("Oven: Heated").run
        Oven()

class BreadHomeMade(
    heat: HeatSource,
    dough: Dough,
) extends Bread

val homeMadeBread =
  ZLayer.fromZIO:
    defer:
      ZIO.debug("BreadHomeMade: Baked").run
      BreadHomeMade(
        ZIO.service[Oven].run,
        ZIO.service[Dough].run,
      )

object App1 extends helpers.ZIOAppDebug:
  def run =
    eatBread.provide(
      homeMadeBread,
      Dough.fresh,
      Oven.heated,
    )
  // Dough: Mixed
  // Oven: Heated
  // BreadHomeMade: Baked
  // Bread: Eating


object Bread:
  val storeBought = storeBoughtBread
  val homeMade    = homeMadeBread

trait Toast:
  val bread: Bread
  val heat: HeatSource
  val eat = ZIO.debug("Toast: Eating")

case class ToastFromHeatSource(
    bread: Bread,
    heat: HeatSource,
) extends Toast

object ToastFromHeatSource:
  val toasted =
    ZLayer.fromZIO:
      defer:
        ZIO.debug("Toast: Made").run
        ToastFromHeatSource(
          ZIO.service[Bread].run,
          ZIO.service[HeatSource].run,
        )

class Toaster extends HeatSource

object Toaster:
  val ready =
    ZLayer.fromZIO:
      defer:
        ZIO.debug("Toaster: Ready").run
        Toaster()

case class ToastFromToaster(
    bread: Bread,
    heat: Toaster,
) extends Toast

object ToastFromToaster:
  val toasted =
    ZLayer.fromZIO:
      defer:
        ZIO.debug("Toast: Made").run
        ToastFromToaster(
          ZIO.service[Bread].run,
          ZIO.service[Toaster].run,
        )

object App2 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .serviceWithZIO[Toast]:
        toast => toast.eat
      .provide(
        ToastFromToaster.toasted,
        Dough.fresh,
        Bread.homeMade,
        // The two HeatSources don't clash:
        Oven.heated,
        Toaster.ready,
      )
  // Toaster: Ready
  // Oven: Heated
  // Dough: Mixed
  // BreadHomeMade: Baked
  // Toast: Made
  // Toast: Eating


object App3 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .serviceWithZIO[Toast]:
        toast => toast.eat
      .provide(
        ZLayer.Debug.tree,
        ToastFromToaster.toasted,
        Dough.fresh,
        Bread.homeMade,
        Oven.heated,
        Toaster.ready,
      )
  // Toaster: Ready
  // Oven: Heated
  // Dough: Mixed
  // BreadHomeMade: Baked
  // Toast: Made
  // Toast: Eating


object OvenSafe:
  val heated =
    ZLayer.scoped:
      defer:
        ZIO.debug("Oven: Heated").run
        Oven()
      .withFinalizer:
        _ => ZIO.debug("Oven: Turning off")

object App4 extends helpers.ZIOAppDebug:
  def run =
    eatBread.provide(
      Bread.homeMade,
      Dough.fresh,
      OvenSafe.heated,
    )
  // Oven: Heated
  // Dough: Mixed
  // BreadHomeMade: Baked
  // Bread: Eating
  // Oven: Turning off


class BreadFromFriend extends Bread()

object Friend:
  def forcedFailure(invocations: Int) =
    defer:
      ZIO
        .debug(
          s"Attempt $invocations: Failure(Friend Unreachable)"
        )
        .run
      ZIO
        .when(true)(
          ZIO.fail(
            "Failure(Friend Unreachable)"
          )
        )
        .as(???)
        .run
      ZIO.succeed(BreadFromFriend()).run

  def requestBread =
    val worksOnAttempt = 4
    var invocations: Ref[Int] =
      Unsafe.unsafe:
        implicit unsafe =>
          Runtime
            .default
            .unsafe
            .run(Ref.make(0))
            .getOrThrow()
    defer:
      val curInvocations =
        invocations.updateAndGet(_ + 1).run
      if curInvocations < worksOnAttempt then
        forcedFailure(curInvocations).run
      else
        ZIO
          .debug(
            s"Attempt $curInvocations: Succeeded"
          )
          .as:
            BreadFromFriend()
          .run
  end requestBread
end Friend

object App5 extends helpers.ZIOAppDebug:
  def run =
    eatBread.provide:
      ZLayer.fromZIO:
        Friend.requestBread
  // Attempt 1: Failure(Friend Unreachable)
  // Error: Failure(Friend Unreachable)


object App6 extends helpers.ZIOAppDebug:
  def run =
    eatBread.provide:
      ZLayer
        .fromZIO:
          Friend.requestBread
        .orElse:
          Bread.storeBought
  // Attempt 1: Failure(Friend Unreachable)
  // Buying bread
  // Bread: Eating


object App7 extends helpers.ZIOAppDebug:
  def run =
    eatBread.provide:
      ZLayer.fromZIO:
        Friend.requestBread.retryN(1)
  // Attempt 1: Failure(Friend Unreachable)
  // Attempt 2: Failure(Friend Unreachable)
  // Error: Failure(Friend Unreachable)


case class RetryConfig(times: Int)

val configurableBread =
  ZLayer.fromZIO:
    defer:
      val config =
        ZIO.service[RetryConfig].run
      Friend
        .requestBread
        .retryN:
          config.times
        .run

object App8 extends helpers.ZIOAppDebug:
  val retryTwice =
    ZLayer.succeed:
      RetryConfig(2)
  
  def run =
    eatBread
      .provide(configurableBread, retryTwice)
  // Attempt 1: Failure(Friend Unreachable)
  // Attempt 2: Failure(Friend Unreachable)
  // Attempt 3: Failure(Friend Unreachable)
  // Error: Failure(Friend Unreachable)


import zio.config.magnolia.deriveConfig

val configDescriptor =
  deriveConfig[RetryConfig]

import zio.config.*
import zio.config.typesafe.*

val configProvider =
  ConfigProvider.fromHoconString:
    "{ times: 3 }"

val configuration =
  ZLayer.fromZIO:
    read:
      configDescriptor.from:
        configProvider

object App9 extends helpers.ZIOAppDebug:
  def run =
    eatBread.provide(
      configurableBread,
      configuration,
    )
  // Attempt 1: Failure(Friend Unreachable)
  // Attempt 2: Failure(Friend Unreachable)
  // Attempt 3: Failure(Friend Unreachable)
  // Attempt 4: Succeeded
  // Bread: Eating
