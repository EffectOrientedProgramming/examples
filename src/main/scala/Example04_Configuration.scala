import zio.*
import zio.direct.*

// Explain private constructor approach
case class Dough():
  val letRise =
    ZIO.debug:
      "Dough is rising"

object Dough:
  val fresh =
    ZLayer.derive[Dough]

case class Heat()

val oven =
  ZLayer.derive[Heat]

trait Bread

case class BreadHomeMade(
    heat: Heat,
    dough: Dough
) extends Bread

object Bread:
  val homemade =
    ZLayer.derive[BreadHomeMade]

case class Toast(heat: Heat, bread: Bread)

object Toast:
  val make =
    ZLayer.derive[Toast]

val toaster =
  ZLayer.derive[Heat]

case class BreadStoreBought() extends Bread

val buyBread =
  ZIO.succeed:
    BreadStoreBought()

val storeBought =
  ZLayer.fromZIO:
    buyBread

case class BreadFromFriend() extends Bread()
object Friend:
  def forcedFailure(invocations: Int) =
    defer:
      Console.printLine(
        s"Attempt $invocations: Error(Friend Unreachable)"
      ).run
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
        Console.printLine(
          s"Attempt $invocations: Succeeded"
        ).orDie.as:
          BreadFromFriend()
end Friend

import zio.config.*
import zio.config.magnolia.deriveConfig
import zio.config.typesafe.*

case class RetryConfig(times: Int)

val configDescriptor: Config[RetryConfig] =
  deriveConfig[RetryConfig]

val configProvider =
  ConfigProvider.fromHoconString:
    "{ times: 2 }"

val config =
  ZLayer.fromZIO:
    read:
      configDescriptor.from:
        configProvider

val coinToss =
  defer:
    if Random.nextBoolean.run then
      ZIO
        .succeed:
          "Heads"
        .run
    else
      ZIO
        .fail:
          "Tails"
        .run

val flipTen =
  defer:
    ZIO
      .collectAllSuccesses:
        List.fill(10):
          coinToss.debug
      .run
      .size

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

object Example04_Configuration_0 extends ZIOAppDefault:
  def run =
    ZIO
      .serviceWithZIO[Dough]:
        dough => dough.letRise
      .provide:
        Dough.fresh
  // Dough is rising
  // Result: ()


object Example04_Configuration_1 extends ZIOAppDefault:
  def run =
    ZIO
      .service[Bread]
      .provide(Bread.homemade, Dough.fresh, oven)
  // Result: BreadHomeMade(Heat(),Dough())


object Example04_Configuration_2 extends ZIOAppDefault:
  def run =
    ZIO
      .service[Toast]
      .provide(
        Toast.make,
        Bread.homemade,
        Dough.fresh,
        oven
      )
  // Result: Toast(Heat(),BreadHomeMade(Heat(),Dough()))


object Example04_Configuration_3 extends ZIOAppDefault:
  def run =
    ZIO
      .service[Heat]
      .provide:
        toaster
  // Result: Heat()


object Example04_Configuration_4 extends ZIOAppDefault:
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
  // Result: Toast(Heat(),BreadHomeMade(Heat(),Dough()))


object Example04_Configuration_5 extends ZIOAppDefault:
  def run =
    ZIO
      .service[Bread]
      .provide:
        storeBought
  // Result: BreadStoreBought()


object Example04_Configuration_6 extends ZIOAppDefault:
  def run =
    ZIO
      .service[Bread]
      .provide:
        Friend.bread(worksOnAttempt =
          3
        )
  // Attempt 1: Error(Friend Unreachable)
  // Result: Error(Friend Unreachable)


object Example04_Configuration_7 extends ZIOAppDefault:
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
  // Result: BreadStoreBought()


object Example04_Configuration_8 extends ZIOAppDefault:
  def run =
    ZIO
      .service[Bread]
      .provide:
        Friend
          .bread(worksOnAttempt =
            3
          )
          .retry:
            Schedule.recurs:
              1
  // Attempt 1: Error(Friend Unreachable)
  // Attempt 2: Error(Friend Unreachable)
  // Result: Error(Friend Unreachable)


object Example04_Configuration_9 extends ZIOAppDefault:
  def run =
    ZIO
      .service[Bread]
      .provide:
        Friend
          .bread(worksOnAttempt =
            3
          )
          .retry:
            Schedule.recurs:
              2
  // Attempt 1: Error(Friend Unreachable)
  // Attempt 2: Error(Friend Unreachable)
  // Attempt 3: Succeeded
  // Result: BreadFromFriend()


object Example04_Configuration_10 extends ZIOAppDefault:
  def run =
    ZIO
      .service[Bread]
      .provide:
        Friend
          .bread(worksOnAttempt =
            3
          )
          .retry:
            Schedule.recurs:
              1
          .orElse:
            storeBought
  // Attempt 1: Error(Friend Unreachable)
  // Attempt 2: Error(Friend Unreachable)
  // Result: BreadStoreBought()


object Example04_Configuration_11 extends ZIOAppDefault:
  def run =
    ZIO
      .serviceWithZIO[RetryConfig]:
        retryConfig =>
          ZIO
            .service[Bread]
            .provide:
              Friend
                .bread(worksOnAttempt =
                  3
                )
                .retry:
                  Schedule.recurs:
                    retryConfig.times
      .provide:
        config
  // Attempt 1: Error(Friend Unreachable)
  // Attempt 2: Error(Friend Unreachable)
  // Attempt 3: Succeeded
  // Result: BreadFromFriend()


object Example04_Configuration_12 extends ZIOAppDefault:
  def run =
    flipTen
  // Heads
  // Heads
  // <FAIL> Fail(Tails,Stack trace for thread "zio-fiber-200573":
  // 	at repl.MdocSession.MdocApp.coinToss(<input>:395)
  // 	at repl.MdocSession.MdocApp.flipTen(<input>:412)
  // 	at zio.direct.ZioMonad.Success.$anon.map(ZioMonad.scala:18)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:22)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:32)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:39)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:46)
  // 	at mdoctools.ToRun.runAndPrintOutput.e(MdocHelpers.scala:55)
  // 	at mdoctools.ToRun.runAndPrintOutput.e(MdocHelpers.scala:56)
  // 	at mdoctools.ToRun.runAndPrintOutput.result(MdocHelpers.scala:63))
  // <FAIL> Fail(Tails,Stack trace for thread "zio-fiber-200573":
  // 	at repl.MdocSession.MdocApp.coinToss(<input>:395)
  // 	at repl.MdocSession.MdocApp.flipTen(<input>:412)
  // 	at zio.direct.ZioMonad.Success.$anon.map(ZioMonad.scala:18)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:22)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:32)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:39)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:46)
  // 	at mdoctools.ToRun.runAndPrintOutput.e(MdocHelpers.scala:55)
  // 	at mdoctools.ToRun.runAndPrintOutput.e(MdocHelpers.scala:56)
  // 	at mdoctools.ToRun.runAndPrintOutput.result(MdocHelpers.scala:63))
  // Heads
  // <FAIL> Fail(Tails,Stack trace for thread "zio-fiber-200573":
  // 	at repl.MdocSession.MdocApp.coinToss(<input>:395)
  // 	at repl.MdocSession.MdocApp.flipTen(<input>:412)
  // 	at zio.direct.ZioMonad.Success.$anon.map(ZioMonad.scala:18)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:22)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:32)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:39)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:46)
  // 	at mdoctools.ToRun.runAndPrintOutput.e(MdocHelpers.scala:55)
  // 	at mdoctools.ToRun.runAndPrintOutput.e(MdocHelpers.scala:56)
  // 	at mdoctools.ToRun.runAndPrintOutput.result(MdocHelpers.scala:63))
  // <FAIL> Fail(Tails,Stack trace for thread "zio-fiber-200573":
  // 	at repl.MdocSession.MdocApp.coinToss(<input>:395)
  // 	at repl.MdocSession.MdocApp.flipTen(<input>:412)
  // 	at zio.direct.ZioMonad.Success.$anon.map(ZioMonad.scala:18)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:22)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:32)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:39)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:46)
  // 	at mdoctools.ToRun.runAndPrintOutput.e(MdocHelpers.scala:55)
  // 	at mdoctools.ToRun.runAndPrintOutput.e(MdocHelpers.scala:56)
  // 	at mdoctools.ToRun.runAndPrintOutput.result(MdocHelpers.scala:63))
  // Heads
  // <FAIL> Fail(Tails,Stack trace for thread "zio-fiber-200573":
  // 	at repl.MdocSession.MdocApp.coinToss(<input>:395)
  // 	at repl.MdocSession.MdocApp.flipTen(<input>:412)
  // 	at zio.direct.ZioMonad.Success.$anon.map(ZioMonad.scala:18)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:22)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:32)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:39)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:46)
  // 	at mdoctools.ToRun.runAndPrintOutput.e(MdocHelpers.scala:55)
  // 	at mdoctools.ToRun.runAndPrintOutput.e(MdocHelpers.scala:56)
  // 	at mdoctools.ToRun.runAndPrintOutput.result(MdocHelpers.scala:63))
  // <FAIL> Fail(Tails,Stack trace for thread "zio-fiber-200573":
  // 	at repl.MdocSession.MdocApp.coinToss(<input>:395)
  // 	at repl.MdocSession.MdocApp.flipTen(<input>:412)
  // 	at zio.direct.ZioMonad.Success.$anon.map(ZioMonad.scala:18)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:22)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:32)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:39)
  // 	at mdoctools.Rendering.renderEveryPossibleOutcomeZio(Rendering.scala:46)
  // 	at mdoctools.ToRun.runAndPrintOutput.e(MdocHelpers.scala:55)
  // 	at mdoctools.ToRun.runAndPrintOutput.e(MdocHelpers.scala:56)
  // 	at mdoctools.ToRun.runAndPrintOutput.result(MdocHelpers.scala:63))
  // Result: 4
