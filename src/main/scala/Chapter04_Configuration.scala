package Chapter04_Configuration

import zio.*
import zio.direct.*

// Explain private constructor approach
case class Dough():
  val letRise =
    Console.printLine:
      "Dough is rising"

object Dough:
  val fresh =
    ZLayer.derive[Dough]
      .tap(_ => Console.printLine("Dough: Mixed"))

object Chapter04_Configuration_0 extends ZIOAppDefault:
  def run =
    ZIO
      .serviceWithZIO[Dough]:
        dough => dough.letRise
      .provide:
        Dough.fresh


case class Heat()

// TODO Version of oven that turns off when finished?
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

object Chapter04_Configuration_1 extends ZIOAppDefault:
  def run =
    ZIO
      .serviceWithZIO[Bread]:
        bread => bread.eat
      .provide(Bread.homemade, Dough.fresh, oven)


case class Toast(heat: Heat, bread: Bread)

object Toast:
  val make =
    ZLayer.derive[Toast]
      .tap(_ => Console.printLine("Toast: Made"))

object Chapter04_Configuration_2 extends ZIOAppDefault:
  def run =
    ZIO
      .service[Toast]
      .provide(
        Toast.make,
        Bread.homemade,
        Dough.fresh,
        oven
      )


val toaster =
  ZLayer.derive[Heat]
   .tap(_ => Console.printLine("Toaster: Heated"))

object Chapter04_Configuration_3 extends ZIOAppDefault:
  def run =
    ZIO
      .service[Heat]
      .provide:
        toaster


object Chapter04_Configuration_4 extends ZIOAppDefault:
  def run =
    ZIO
      .serviceWithZIO[Bread]:
        bread =>
          ZIO
            .service[Toast]
            .provide(
              Toast.make,
              toaster,
              ZLayer.succeed:
                bread
            )
      .provide(Bread.homemade, Dough.fresh, oven)


case class BreadStoreBought() extends Bread

val buyBread =
  ZIO.succeed:
    BreadStoreBought()

val storeBought =
  ZLayer.fromZIO:
    buyBread
  .tap(_ => Console.printLine("BreadStoreBought: Bought"))

object Chapter04_Configuration_5 extends ZIOAppDefault:
  def run =
    ZIO
      .service[Bread]
      .provide:
        storeBought


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

object Chapter04_Configuration_6 extends ZIOAppDefault:
  def run =
    ZIO
      .service[Bread]
      .provide:
        Friend.bread(worksOnAttempt =
          3
        )


object Chapter04_Configuration_7 extends ZIOAppDefault:
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
  

object Chapter04_Configuration_8 extends ZIOAppDefault:
  def run =
    logicWithRetries(retries = 1)


object Chapter04_Configuration_9 extends ZIOAppDefault:
  def run =
    logicWithRetries(retries = 2)


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

object Chapter04_Configuration_10 extends ZIOAppDefault:
  def run =
    ZIO
      .serviceWithZIO[RetryConfig]:
        retryConfig =>
          logicWithRetries(
            retries = retryConfig.times
          )
      .provide:
        config


object Chapter04_Configuration_11 extends ZIOAppDefault:
  // TODO Split this up? It's pretty busy.
  // TODO Can we introduce acquireRelease in isolation in superpowers?
  val ovenSafe =
    ZLayer.fromZIO:
      ZIO.acquireRelease(
        ZIO.succeed(Heat())
          .tap(_ => Console.printLine("Oven: Heated"))
      )(
        oven => 
          Console.printLine("Oven: Turning off!").orDie
      )


object Chapter04_Configuration_12 extends ZIOAppDefault:
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


val coinToss =
  // TODO: This is the first place we use defer.
  // We need to deliberately, and explicitly,
  // introduce it.
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

object Chapter04_Configuration_13 extends ZIOAppDefault:
  def run =
    flipTen


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