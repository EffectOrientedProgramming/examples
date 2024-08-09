package Chapter04_Initialization

import zio.*
import zio.direct.*

trait Bread:
  def eat =
    ZIO.debug("Bread: Eating")

class BreadStoreBought extends Bread

object BreadStoreBought:
  val purchase =
    ZIO.succeed:
      BreadStoreBought()

val eatBread =
  ZIO.serviceWithZIO[Bread]:
    bread => bread.eat

object App0 extends helpers.ZIOAppDebug:
  def run =
    eatBread.provide:
      ZLayer.fromZIO:
        BreadStoreBought.purchase
  // Bread: Eating


class X:
  val display = ZIO.debug("X.display")

val makeX =
  defer:
    ZIO.debug("Creating X").run
    X()

val dependency =
  ZLayer.fromZIO:
    makeX

object X:
  val layer =
    ZLayer.fromZIO:
      makeX

object App1 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .serviceWithZIO[X]:
        x => x.display
      .provide:
        X.layer
  // Creating X
  // X.display


class Y:
  val display = ZIO.debug("Y.display")

val makeY =
  defer:
    ZIO.debug("makeY.run creating Y()").run
    Y()

object Y:
  val layer =
    ZLayer.fromZIO:
      makeY

def _type(obj: Any): String =
  obj.getClass.getName.split("\\$")(0)

def showType(id: String, obj: Any) =
  ZIO.debug(s"$id is a ${_type(obj)}")

object App2 extends helpers.ZIOAppDebug:
  def show(y: Y) =
    defer:
      ZIO.debug(s"y: $y").run
      y.display.run
  
  val main =
    ZIO
      .serviceWithZIO[Y]:
        y => show(y)
      .provide:
        Y.layer
  
  def run =
    defer:
      showType("makeY", makeY).run
      val y = makeY.run
      ZIO.debug(s"makeY.run returned $y").run
      showType("Y.layer", Y.layer).run
      showType("main", main).run
      main.run
      ZIO.debug("main.run complete").run
  // makeY is a zio.ZIO
  // makeY.run creating Y()
  // makeY.run returned repl.MdocSession$MdocApp$Y@1cf73b8b
  // Y.layer is a zio.ZLayer
  // main is a zio.ZIO
  // makeY.run creating Y()
  // y: repl.MdocSession$MdocApp$Y@5f2664f3
  // Y.display
  // main.run complete


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

object App3 extends helpers.ZIOAppDebug:
  def run =
    eatBread.provide(
      BreadHomeMade.baked,
      Dough.fresh,
      Oven.heated,
    )
  // Oven: Heated
  // Dough: Mixed
  // BreadHomeMade: Baked
  // Bread: Eating


trait Toast:
  def bread: Bread
  def heat: HeatSource
  val eat = ZIO.debug("Toast: Eating")

case class ToastA(
    heat: HeatSource,
    bread: Bread,
) extends Toast

object ToastA:
  val toasted =
    ZLayer.fromZIO:
      defer:
        ZIO.debug("ToastA: Made").run
        ToastA(
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

case class ToastB(
    heat: Toaster,
    bread: Bread,
) extends Toast
// ToastA used HeatSource for heat

object ToastB:
  val toasted =
    ZLayer.fromZIO:
      defer:
        ZIO.debug("ToastB: Made").run
        ToastB(
          ZIO.service[Toaster].run,
          ZIO.service[Bread].run,
        )

object App4 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .serviceWithZIO[Toast]:
        toast => toast.eat
      .provide(
        ToastB.toasted,
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
  // ToastB: Made
  // Toast: Eating


object OvenSafe:
  val heated =
    ZLayer.scoped:
      defer:
        ZIO.debug("Oven: Heated").run
        Oven()
      .withFinalizer:
        _ => ZIO.debug("Oven: Turning off")

object App5 extends helpers.ZIOAppDebug:
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
      else if curInvocations == 1 then
        ZIO.succeed(BreadFromFriend()).run
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

object App6 extends helpers.ZIOAppDebug:
  // TODO Review supporting prose of everything from Friend.requestBread down. Significantly reworked this sequence.
  
  def run =
    eatBread.provide:
      ZLayer.fromZIO:
        Friend.requestBread
  // Attempt 1: Failure(Friend Unreachable)
  // Result: Failure(Friend Unreachable)


object App7 extends helpers.ZIOAppDebug:
  def run =
    eatBread.provide:
      ZLayer.fromZIO:
        Friend
          .requestBread
          .orElse:
            BreadStoreBought.purchase
  // Attempt 1: Failure(Friend Unreachable)
  // Bread: Eating


object App8 extends helpers.ZIOAppDebug:
  def run =
    eatBread.provide:
      ZLayer.fromZIO:
        Friend
          .requestBread
          .retryN:
            1
  // Attempt 1: Failure(Friend Unreachable)
  // Attempt 2: Failure(Friend Unreachable)
  // Result: Failure(Friend Unreachable)


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

object App9 extends helpers.ZIOAppDebug:
  def run =
    eatBread.provide(
      configurableBread,
      ZLayer.succeed:
        RetryConfig(2),
    )
  // Attempt 1: Failure(Friend Unreachable)
  // Attempt 2: Failure(Friend Unreachable)
  // Attempt 3: Failure(Friend Unreachable)
  // Result: Failure(Friend Unreachable)


import zio.config.*

import zio.config.magnolia.deriveConfig

val configDescriptor =
  deriveConfig[RetryConfig]

import zio.config.typesafe.*

val configProvider =
  ConfigProvider.fromHoconString:
    "{ times: 2 }"

val configuration =
  ZLayer.fromZIO:
    read:
      configDescriptor.from:
        configProvider

object App10 extends helpers.ZIOAppDebug:
  def run =
    eatBread.provide(
      configurableBread,
      configuration,
    )
  // Attempt 1: Failure(Friend Unreachable)
  // Attempt 2: Failure(Friend Unreachable)
  // Attempt 3: Failure(Friend Unreachable)
  // Result: Failure(Friend Unreachable)
