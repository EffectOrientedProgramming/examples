package Chapter04_Initialization

import zio.*
import zio.direct.*

// TODO "Defining Effect Dependencies" is ambiguous/strange.
//   I think it should just be "Defining Dependencies"
//   It might still be off though. What we are really doing is defining layers/modules/units that _can_ be dependencies
//   I'm not sure we can say that they are intrinsincally dependencies

trait Bread:
  val eat = ZIO.debug("Bread: Eating")

// TODO I get that this doesn't technically _need_ parens after the class definition
//    But it could be more straightforward to see the constructor with parens used
//    below if we also had them here

class BreadStoreBought extends Bread

val someLayer =
  ZLayer.succeed:
    BreadStoreBought()

val purchaseBread =
  defer:
    ZIO.debug("Buying bread").run
    BreadStoreBought()

object BreadStoreBought:
  val layer =
    ZLayer.fromZIO:
      purchaseBread

/* TODO The noun object + adjective val claim is *not* shown by the previous `BreadStoreBought.layer`
 *   I want to give an example like:
 *
 *   object Bread:
 *     val storeBought = ZLayer.fromZIO(???)
 *     val homeMade = ZLayer.fromZIO(???)
 *   
 *   But don't want it to be confused with the real `Bread` structure we are using elsewhere.
 *   Should we briefly use a throwaway example like:
 *
 *   object Server:
 *     val cheap = ZLayer.fromZIO(???)
 *     val expensive = ZLayer.fromZIO(???)
 */

val eatBread =
  ZIO.serviceWithZIO[Bread]:
    bread => bread.eat

// TODO Explain "initialization specification", or come up with a simpler term. 
//    We use it and assume sort of definition, but do not provide one.
//    Possibly just "initialization" or "initialization logic/steps"

object App0 extends helpers.ZIOAppDebug:
  def run =
    eatBread.provide:
      BreadStoreBought.layer
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

object BreadHomeMade:
  val baked =
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
      BreadHomeMade.baked,
      Dough.fresh,
      Oven.heated,
      // ZLayer.Debug.tree,
    )
  // Oven: Heated
  // Dough: Mixed
  // BreadHomeMade: Baked
  // Bread: Eating


trait Toast:
  val bread: Bread
  val heat: HeatSource
  val eat = ZIO.debug("Toast: Eating")

case class ToastFromHeatSource(
    heat: HeatSource,
    bread: Bread,
) extends Toast

object ToastFromHeatSource:
  val toasted =
    ZLayer.fromZIO:
      defer:
        ZIO.debug("Toast: Made").run
        ToastFromHeatSource(
          ZIO.service[HeatSource].run,
          ZIO.service[Bread].run,
        )

class Toaster extends HeatSource

object Toaster:
  val ready =
    ZLayer.fromZIO:
      defer:
        ZIO.debug("Toaster: Ready").run
        Toaster()

case class ToastFromToaster(
    heat: Toaster,
    bread: Bread,
) extends Toast

object ToastFromToaster:
  val toasted =
    ZLayer.fromZIO:
      defer:
        ZIO.debug("Toast: Made").run
        ToastFromToaster(
          ZIO.service[Toaster].run,
          ZIO.service[Bread].run,
        )

object App2 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .serviceWithZIO[Toast]:
        toast => toast.eat
      .provide(
        ToastFromToaster.toasted,
        Dough.fresh,
        BreadHomeMade.baked,
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


def run =
  ZIO
    .serviceWithZIO[Toast]:
      toast => toast.eat
    .provide(
      ZLayer.Debug.tree,
      ToastFromToaster.toasted,
      Dough.fresh,
      BreadHomeMade.baked,
      // The two HeatSources don't clash:
      Oven.heated,
      Toaster.ready,
    )

object OvenSafe:
  val heated =
    ZLayer.scoped:
      defer:
        ZIO.debug("Oven: Heated").run
        Oven()
      .withFinalizer:
        _ => ZIO.debug("Oven: Turning off")

object App3 extends helpers.ZIOAppDebug:
  def run =
    eatBread.provide(
      BreadHomeMade.baked,
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

object App4 extends helpers.ZIOAppDebug:
  def run =
    eatBread.provide:
      ZLayer.fromZIO:
        Friend.requestBread
  // Attempt 1: Failure(Friend Unreachable)
  // Error: Failure(Friend Unreachable)


object App5 extends helpers.ZIOAppDebug:
  def run =
    eatBread.provide:
      ZLayer
        .fromZIO:
          Friend.requestBread
        .orElse:
          BreadStoreBought.layer
  // Attempt 1: Failure(Friend Unreachable)
  // Buying bread
  // Bread: Eating


object App6 extends helpers.ZIOAppDebug:
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

object App7 extends helpers.ZIOAppDebug:
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

object App8 extends helpers.ZIOAppDebug:
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
