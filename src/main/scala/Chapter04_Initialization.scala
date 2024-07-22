package Chapter04_Initialization

import zio.*
import zio.direct.*
import zio.Console.*

trait Bread:
  def eat =
    printLine("Bread: Eating")

class BreadStoreBought extends Bread

object BreadStoreBought:
  val purchased =
    ZLayer.succeed:
      BreadStoreBought()

val eatBread =
  ZIO.serviceWithZIO[Bread]:
    bread => bread.eat

object App0 extends helpers.ZIOAppDebug:
  def run =
    eatBread.provide:
      BreadStoreBought.purchased
  // Bread: Eating


class X:
  val display = printLine("X.display")

val makeX =
  defer:
    printLine("Creating X").run
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
  val display = printLine("Y.display")

val makeY =
  defer:
    printLine("makeY.run creating Y()").run
    Y()

object Y:
  val layer =
    ZLayer.fromZIO:
      makeY

def _type(obj: Any): String =
  obj.getClass.getName.split("\\$")(0)

def showType(id: String, obj: Any) =
  printLine(s"$id is a ${_type(obj)}")

object App2 extends helpers.ZIOAppDebug:
  def show(y: Y) =
    defer:
      printLine(s"y: $y").run
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
      printLine(s"makeY.run returned $y").run
      showType("Y.layer", Y.layer).run
      showType("main", main).run
      main.run
      printLine("main.run complete").run
  // makeY is a zio.ZIO
  // makeY.run creating Y()
  // makeY.run returned repl.MdocSession$MdocApp$Y@b712d80
  // Y.layer is a zio.ZLayer
  // main is a zio.ZIO
  // makeY.run creating Y()
  // y: repl.MdocSession$MdocApp$Y@29a6c925
  // Y.display
  // main.run complete


class Dough:
  val letRise = printLine("Dough: rising")

object Dough:
  val fresh =
    ZLayer.fromZIO:
      defer:
        printLine("Dough: Mixed").run
        Dough()

trait HeatSource
class Oven extends HeatSource

object Oven:
  val heated =
    ZLayer.fromZIO:
      defer:
        printLine("Oven: Heated").run
        Oven()

class BreadHomeMade(
    heat: HeatSource,
    dough: Dough,
) extends Bread

object BreadHomeMade:
  val baked =
    ZLayer.fromZIO:
      defer:
        printLine("BreadHomeMade: Baked").run
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
  val eat = printLine("Toast: Eating")

case class ToastA(
    heat: HeatSource,
    bread: Bread,
) extends Toast

object ToastA:
  val toasted =
    ZLayer.fromZIO:
      defer:
        printLine("ToastA: Made").run
        ToastA(
          ZIO.service[HeatSource].run,
          ZIO.service[Bread].run,
        )

class Toaster extends HeatSource

object Toaster:
  val ready =
    ZLayer.fromZIO:
      defer:
        printLine("Toaster: Ready").run
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
        printLine("ToastB: Made").run
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
  // Dough: Mixed
  // Oven: Heated
  // BreadHomeMade: Baked
  // ToastB: Made
  // Toast: Eating


object OvenSafe:
  val heated =
    ZLayer.scoped:
      defer:
        printLine("Oven: Heated").run
        Oven()
      .withFinalizer:
        _ =>
          printLine("Oven: Turning off")
            .orDie

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
    var invocations = 0
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

object App6 extends helpers.ZIOAppDebug:
  def run =
    eatBread.provide:
      Friend.bread(worksOnAttempt = 3)
  // Attempt 1: Failure(Friend Unreachable)
  // Result: Failure(Friend Unreachable)


object App7 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .service[Bread]
      .provide:
        Friend
          .bread(worksOnAttempt = 3)
          .orElse:
            BreadStoreBought.purchased
  // Attempt 1: Failure(Friend Unreachable)
  // TODO Handle long line. 
  // Truncating for now: 
  // repl.MdocSession$MdocApp$BreadStoreBought@7aba3d3
  // Result: repl.MdocSession$MdocApp$BreadStoreBought@7aba


def eatEatEat(retries: Int) =
  eatBread.provide:
    Friend
      .bread(worksOnAttempt = 3)
      .retry:
        Schedule.recurs:
          retries

object App8 extends helpers.ZIOAppDebug:
  def run =
    eatEatEat(retries = 2)
  // Attempt 1: Failure(Friend Unreachable)
  // Attempt 2: Failure(Friend Unreachable)
  // Attempt 3: Succeeded
  // Bread: Eating


import zio.config.*

case class RetryConfig(
    times: Int,
    msg: String,
)

import zio.config.magnolia.deriveConfig

val configDescriptor =
  deriveConfig[RetryConfig]

import zio.config.typesafe.*

val configProvider =
  ConfigProvider.fromHoconString:
    "{ times: 2, msg: Trying to eat bread }"

val configuration =
  ZLayer.fromZIO:
    read:
      configDescriptor.from:
        configProvider

object App9 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .serviceWithZIO[RetryConfig]:
        retryConfig =>
          val times = retryConfig.times
          val msg   = retryConfig.msg
          println(msg)
          println(s"Retrying $times times")
          eatEatEat(retries = times)
      .provide:
        configuration
  // Trying to eat bread
  // Retrying 2 times
  // Attempt 1: Failure(Friend Unreachable)
  // Attempt 2: Failure(Friend Unreachable)
  // Attempt 3: Succeeded
  // Bread: Eating
