import zio.*
import zio.direct.*

import zio.{ZIO, ZLayer}
import zio.cache.{Cache, Lookup}

import java.nio.file.Path

case class FSLive(requests: Ref[Int])
    extends CloudStorage:
  def retrieve(
      name: Path
  ): ZIO[Any, Nothing, FileContents] =
    defer:
      requests.update(_ + 1).run
      ZIO.sleep(10.millis).run
      FSLive.hardcodedContents

  val invoice: ZIO[Any, Nothing, String] =
    defer:
      val count =
        requests.get.run

      "Amount owed: $" + count

object FSLive:
  val hardcodedContents =
    FileContents(
      List("viralImage1", "viralImage2")
    )

case class FileContents(contents: List[String])

trait CloudStorage:
  def retrieve(
      name: Path
  ): ZIO[Any, Nothing, FileContents]
  val invoice: ZIO[Any, Nothing, String]

object CloudStorage:
  val live =
    ZLayer.fromZIO:
      defer:
        FSLive(Ref.make(0).run)

case class PopularService(
    retrieveContents: Path => ZIO[
      Any,
      Nothing,
      FileContents
    ]
):
  def retrieve(name: Path) =
    retrieveContents(name)

val thunderingHerdsScenario =
  defer:
    val popularService =
      ZIO.service[PopularService].run

    ZIO // All requests arrives nearly at once
      .foreachPar(List.fill(100)(())):
        _ => // james don't like
          popularService.retrieve:
            Path.of("awesomeMemes")
      .run

    val cloudStorage =
      ZIO.service[CloudStorage].run

    cloudStorage.invoice.debug.run

val makePopularService =
  defer:
    val cloudStorage =
      ZIO.service[CloudStorage].run
    PopularService(cloudStorage.retrieve)

val popularService =
  ZLayer.fromZIO(makePopularService)

val makeCachedPopularService =
  defer:
    val cloudStorage =
      ZIO.service[CloudStorage].run
    val cache =
      Cache
        .make(
          capacity =
            100,
          timeToLive =
            Duration.Infinity,
          lookup =
            Lookup(cloudStorage.retrieve)
        )
        .run

    PopularService(cache.get)

val expensiveApiCall =
  ZIO.unit

extension [R, E, A](z: ZIO[R, E, A])
  def timedSecondsDebug(
      message: String
  ): ZIO[R, E, A] =
    z.timed
      .tap:
        (duration, _) =>
          Console.printLine(
            message + " [took " +
              duration.getSeconds + "s]"
          ).orDie
      .map(_._2)

import nl.vroste.rezilience.RateLimiter

val makeRateLimiter =
  RateLimiter.make(
    max =
      1,
    interval =
      1.second
  )

// shows extension function definition
// so that we can explain timedSecondsDebug
extension (rateLimiter: RateLimiter)
  def makeCalls(name: String) =
    rateLimiter:
      expensiveApiCall
    .timedSecondsDebug:
      s"$name called API"
    .repeatN(2) // Repeats as fast as allowed

trait DelicateResource:
  val request: ZIO[Any, String, Int]
end DelicateResource

// It can represent any service outside of our control
// that has usage constraints
case class Live(
    currentRequests: Ref[List[Int]],
    alive: Ref[Boolean]
) extends DelicateResource:
  val request =
    defer:
      val res =
        Random.nextIntBounded(1000).run

      if (currentRequests.get.run.length > 3)
        alive.set(false).run
        ZIO.fail("Killed the server!!").run

      // Add request to current requests
      currentRequests
        .updateAndGet(res :: _)
        .debug("Current requests: ")
        .run

      // Simulate a long-running request
      ZIO.sleep(1.second).run
      removeRequest(res).run

      if (alive.get.run)
        res
      else
        ZIO
          .fail(
            "Server was killed by another request!!"
          )
          .run

  private def removeRequest(i: Int) =
    currentRequests.update(_ diff List(i))

object DelicateResource:
  val live =
    ZLayer.fromZIO:
      defer:
        Console.printLine:
          "Delicate Resource constructed."
        .run
        Console.printLine:
          "Do not make more than 3 concurrent requests!"
        .run
        Live(
          Ref.make[List[Int]](List.empty).run,
          Ref.make(true).run
        )

import zio.Ref

import java.time.Instant
import scala.concurrent.TimeoutException

// Invisible mdoc fencess
import zio.Runtime.default.unsafe
val timeSensitiveValue =
  Unsafe.unsafe(
    (u: Unsafe) =>
      given Unsafe =
        u
      unsafe
        .run(
          scheduledValues(
            (1100.millis, true),
            (4100.millis, false),
            (5000.millis, true)
          )
        )
        .getOrThrowFiberFailure()
  )

def externalSystem(numCalls: Ref[Int]) =
  defer:
    numCalls.update(_ + 1).run
    val b =
      timeSensitiveValue.run
    if b then
      ZIO.succeed(()).run
    else
      ZIO.fail(()).run

// TODO Consider deleting
object InstantOps:
  extension (i: Instant)
    def plusZ(duration: zio.Duration): Instant =
      i.plus(duration.asJava)

import InstantOps._

/* Goal: If I accessed this from:
 * 0-1 seconds, I would get "First Value" 1-4
 * seconds, I would get "Second Value" 4-14
 * seconds, I would get "Third Value" 14+
 * seconds, it would fail */

// TODO Consider TimeSequence as a name
def scheduledValues[A](
    value: (Duration, A),
    values: (Duration, A)*
): ZIO[
  Any, // construction time
  Nothing,
  ZIO[
    Any, // access time
    TimeoutException,
    A
  ]
] =
  defer {
    val startTime =
      Clock.instant.run
    val timeTable =
      createTimeTableX(
        startTime,
        value,
        values* // Yay Scala3 :)
      )
    accessX(timeTable)
  }

// TODO Some comments, tests, examples, etc to
// make this function more obvious
private def createTimeTableX[A](
    startTime: Instant,
    value: (Duration, A),
    values: (Duration, A)*
): Seq[ExpiringValue[A]] =
  values.scanLeft(
    ExpiringValue(
      startTime.plusZ(value._1),
      value._2
    )
  ) {
    case (
          ExpiringValue(elapsed, _),
          (duration, value)
        ) =>
      ExpiringValue(
        elapsed.plusZ(duration),
        value
      )
  }

/** Input: (1 minute, "value1") (2 minute,
  * "value2")
  *
  * Runtime: Zero value: (8:00 + 1 minute,
  * "value1")
  *
  * case ((8:01, _) , (2.minutes, "value2")) =>
  * (8:01 + 2.minutes, "value2")
  *
  * Output: ( ("8:01", "value1"), ("8:03",
  * "value2") )
  */
private def accessX[A](
    timeTable: Seq[ExpiringValue[A]]
): ZIO[Any, TimeoutException, A] =
  defer {
    val now =
      Clock.instant.run
    ZIO
      .getOrFailWith(
        new TimeoutException("TOO LATE")
      ) {
        timeTable
          .find(_.expirationTime.isAfter(now))
          .map(_.value)
      }
      .run
  }

private case class ExpiringValue[A](
    expirationTime: Instant,
    value: A
)

val repeatSchedule =
  Schedule.recurs(140) &&
    Schedule.spaced(50.millis)

import nl.vroste.rezilience.{
  CircuitBreaker,
  TrippingStrategy,
  Retry
}
import nl.vroste.rezilience.CircuitBreaker.CircuitBreakerOpen

val makeCircuitBreaker =
  CircuitBreaker.make(
    trippingStrategy =
      TrippingStrategy.failureCount(maxFailures =
        2
      ),
    resetPolicy =
      Retry.Schedules.common()
  )

val logicThatSporadicallyLocksUp =
  defer:
    val random =
      Random.nextIntBounded(1_000).run
    random match
      case 0 =>
        ZIO
          .sleep:
            3.seconds
          .run
        ZIO
          .succeed:
            2.second
          .run
      case _ =>
        10.millis

object Example08_Reliability_0 extends ZIOAppDefault:
  def run =
    thunderingHerdsScenario
      .provide(CloudStorage.live, popularService)
  // Amount owed: $100
  // Result: Success(Amount owed: $100)


object Example08_Reliability_1 extends ZIOAppDefault:
  def run =
    thunderingHerdsScenario.provide(
      CloudStorage.live,
      ZLayer.fromZIO(makeCachedPopularService)
    )
  // Amount owed: $1
  // Result: Success(Amount owed: $1)


object Example08_Reliability_2 extends ZIOAppDefault:
  def run =
    defer:
      val rateLimiter =
        makeRateLimiter.run
      rateLimiter
        .makeCalls:
          "System"
        .timedSecondsDebug("Result")
        .run
  // System called API [took 0s]
  // System called API [took 1s]
  // System called API [took 0s]
  // Result [took 0s]
  // Result: Success(())


object Example08_Reliability_3 extends ZIOAppDefault:
  def run =
    defer:
      val rateLimiter =
        makeRateLimiter.run
      val people =
        List("Bill", "Bruce", "James")
  
      ZIO
        .foreachPar(people):
          rateLimiter.makeCalls
        .timedSecondsDebug:
          "Total time"
        .run
  // Bill called API [took 0s]
  // Bill called API [took 1s]
  // Bill called API [took 0s]
  // James called API [took 0s]
  // James called API [took 1s]
  // James called API [took 0s]
  // Bruce called API [took 0s]
  // Bruce called API [took 1s]
  // Bruce called API [took 0s]
  // Total time [took 0s]
  // Result: Success(List((), (), ()))


object Example08_Reliability_4 extends ZIOAppDefault:
  def run =
    defer:
      val delicateResource =
        ZIO.service[DelicateResource].run
      ZIO
        .foreachPar(1 to 10):
          _ =>
            //          bulkhead:
            delicateResource.request
        .as("All Requests Succeeded!")
        .run
    .provideSome[Scope]:
      DelicateResource.live
  // Delicate Resource constructed.
  // Do not make more than 3 concurrent requests!
  // Current requests: : List(903)
  // Current requests: : List(155)
  // Current requests: : List(376)
  // Current requests: : List(288)
  // Current requests: : List(434)
  // Current requests: : List(254)
  // Current requests: : List(596)
  // Current requests: : List(46)
  // Current requests: : List(104)
  // Current requests: : List(142)
  // Result: Success(All Requests Succeeded!)


object Example08_Reliability_5 extends ZIOAppDefault:
  import nl.vroste.rezilience.Bulkhead
  
  def run =
    defer:
      val bulkhead: Bulkhead =
        Bulkhead
          .make(maxInFlightCalls =
            3
          )
          .run
      val delicateResource =
        ZIO.service[DelicateResource].run
      ZIO
        .foreachPar(1 to 10):
          _ =>
            bulkhead:
              delicateResource.request
        .as("All Requests Succeeded")
        .run
    .provideSome[Scope]:
      DelicateResource.live
  // Delicate Resource constructed.
  // Do not make more than 3 concurrent requests!
  // Current requests: : List(39)
  // Current requests: : List(457, 39)
  // Current requests: : List(587)
  // Current requests: : List(688)
  // Current requests: : List(796)
  // Current requests: : List(287)
  // Current requests: : List(957)
  // Current requests: : List(443)
  // Current requests: : List(666)
  // Current requests: : List(475)
  // Result: Success(All Requests Succeeded)


object Example08_Reliability_6 extends ZIOAppDefault:
  def run =
    defer:
      val numCalls =
        Ref.make[Int](0).run
  
      externalSystem(numCalls)
        .ignore
        .repeat(repeatSchedule)
        .run
  
      val made =
        numCalls.get.run
  
      s"Calls made: $made"
  // Result: Success(Calls made: 141)


object Example08_Reliability_7 extends ZIOAppDefault:
  def run =
    defer:
      val cb =
        makeCircuitBreaker.run
      val numCalls =
        Ref.make[Int](0).run
      val numPrevented =
        Ref.make[Int](0).run
      val protectedCall =
        cb(externalSystem(numCalls)).catchSome:
          case CircuitBreakerOpen =>
            numPrevented.update(_ + 1)
  
      protectedCall
        .ignore
        .repeat(repeatSchedule)
        .run
  
      val prevented =
        numPrevented.get.run
  
      val made =
        numCalls.get.run
      s"Calls prevented: $prevented Calls made: $made"
  // Result: Success(Calls prevented: 0 Calls made: 141)


object Example08_Reliability_8 extends ZIOAppDefault:
  def run =
    defer:
      val contractBreaches =
        Ref.make(0).run
  
      ZIO
        .foreachPar(List.fill(50_000)(())):
          _ => // james still hates this
            defer:
              val hedged =
                logicThatSporadicallyLocksUp.race:
                  logicThatSporadicallyLocksUp
                    .delay:
                      25.millis
  
              // TODO How do we make this demo more
              // obvious?
              // The request is returning the
              // hypothetical runtime, but that's
              // not clear from the code that will
              // be visible to the reader.
              val duration =
                hedged.run
              if (duration > 1.second)
                contractBreaches.update(_ + 1).run
        .run
  
      contractBreaches
        .get
        .debug("Contract Breaches")
        .run
  // Contract Breaches: 47
  // Result: Success(47)
