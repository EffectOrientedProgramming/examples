package Chapter08_Reliability

import zio.*
import zio.direct.*

// TODO: really "advanced recover techniques" as basic ones should have already been covered

import zio.{ZIO, ZLayer}
import zio.cache.{Cache, Lookup}

import java.nio.file.{Path, Paths}

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

case class FileContents(
    contents: List[String]
)

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

val thunderingHerds =
  defer:
    val popularService =
      ZIO.service[PopularService].run

    // All requests arrive at once
    ZIO
      .collectAllPar:
        List.fill(100):
          popularService.retrieve:
            Paths.get("awesomeMemes")
      .run

    val cloudStorage =
      ZIO.service[CloudStorage].run

    cloudStorage.invoice.run

val makePopularService =
  defer:
    val cloudStorage =
      ZIO.service[CloudStorage].run
    PopularService(cloudStorage.retrieve)

object App0 extends helpers.ZIOAppDebug:
  def run =
    thunderingHerds.provide(
      CloudStorage.live,
      ZLayer.fromZIO(makePopularService)
    )
  // Result: Amount owed: $100


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

object App1 extends helpers.ZIOAppDebug:
  def run =
    thunderingHerds.provide(
      CloudStorage.live,
      ZLayer.fromZIO(makeCachedPopularService)
    )
  // Result: Amount owed: $1


val expensiveApiCall =
  ZIO.unit

extension [R, E, A](z: ZIO[R, E, A])
  def timedSecondsDebug(
      message: String
  ): ZIO[R, E, A] =
    z.timed
      .tap:
        (duration, _) =>
          Console
            .printLine(
              message + " [took " +
                duration.getSeconds + "s]"
            )
            .orDie
      .map(_._2)

import nl.vroste.rezilience.RateLimiter

val makeRateLimiter =
  RateLimiter.make(
    max =
      1,
    interval =
      1.second
  )

object App2 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val rateLimiter =
        makeRateLimiter.run
      rateLimiter:
        expensiveApiCall
      .timedSecondsDebug:
        s"called API"
      .repeatN(2) // Repeats as fast as allowed
        .timedSecondsDebug("Result").run
  // called API [took 0s]
  // called API [took 1s]
  // called API [took 1s]
  // Result [took 2s]


object App3 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val rateLimiter =
        makeRateLimiter.run
      val people =
        List("Bill", "Bruce", "James")
  
      ZIO
        .foreachPar(people):
          person =>
            rateLimiter:
              expensiveApiCall
            .timedSecondsDebug:
              s"$person called API"
            .repeatN(
              2
            ) // Repeats as fast as allowed
        .timedSecondsDebug:
          "Total time"
        .unit // ignores the list of unit
        .run
  // Bruce called API [took 0s]
  // James called API [took 1s]
  // Bruce called API [took 2s]
  // Bill called API [took 3s]
  // James called API [took 3s]
  // Bruce called API [took 3s]
  // Bill called API [took 3s]
  // James called API [took 3s]
  // Bill called API [took 2s]
  // Total time [took 8s]


trait DelicateResource:
  val request: ZIO[Any, String, Int]

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
        ZIO.fail("Crashed the server!!").run

      // Add request to current requests
      currentRequests
        .updateAndGet(res :: _)
        .debug("Current requests")
        .run

      // Simulate a long-running request
      ZIO.sleep(1.second).run
      removeRequest(res).run

      if (alive.get.run)
        res
      else
        ZIO
          .fail(
            "Server crashed from requests!!"
          )
          .run

  private def removeRequest(i: Int) =
    currentRequests.update(_ diff List(i))
end Live

object DelicateResource:
  val live =
    ZLayer.fromZIO:
      defer:
        Console
          .printLine:
            "Delicate Resource constructed."
          .run
        Console
          .printLine:
            "Do not make more than 3 concurrent requests!"
          .run
        Live(
          Ref
            .make[List[Int]](List.empty)
            .run,
          Ref.make(true).run
        )

object App4 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val delicateResource =
        ZIO.service[DelicateResource].run
      ZIO
        .foreachPar(1 to 10):
          _ => delicateResource.request
        .as("All Requests Succeeded!")
        .run
    .provide(DelicateResource.live)
  // Delicate Resource constructed.
  // Do not make more than 3 concurrent requests!
  // Current requests: List(941)
  // Current requests: List(907, 941)
  // Current requests: List(367, 907, 941)
  // Current requests: List(451, 367, 907, 941)
  // Current requests: List(676, 451, 367, 907, 941)
  // Result: Crashed the server!!


import nl.vroste.rezilience.Bulkhead
val makeOurBulkhead =
  Bulkhead.make(maxInFlightCalls =
    3
  )

object App5 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val bulkhead =
        makeOurBulkhead.run
  
      val delicateResource =
        ZIO.service[DelicateResource].run
      ZIO
        .foreachPar(1 to 10):
          _ =>
            bulkhead:
              delicateResource.request
        .as("All Requests Succeeded")
        .run
    .provide(
      DelicateResource.live,
      Scope.default
    )
  // Delicate Resource constructed.
  // Do not make more than 3 concurrent requests!
  // Current requests: List(427)
  // Current requests: List(317, 427)
  // Current requests: List(924, 317, 427)
  // Current requests: List(600, 646)
  // Current requests: List(646)
  // Current requests: List(349, 600, 646)
  // Current requests: List(986, 349)
  // Current requests: List(367, 986, 349)
  // Current requests: List(15, 367, 986)
  // Current requests: List(793, 15)
  // Result: All Requests Succeeded


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
      ZIO.unit.run
    else
      ZIO.fail(()).run

object InstantOps:
  extension (i: Instant)
    def plusZ(
        duration: zio.Duration
    ): Instant =
      i.plus(duration.asJava)

import InstantOps._

/* Goal: If I accessed this from:
 * 0-1 seconds, I would get "First Value" 1-4
 * seconds, I would get "Second Value" 4-14
 * seconds, I would get "Third Value" 14+
 * seconds, it would fail */

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
  * case ((8:01, _) , (2.minutes, "value2"))
  * \=> (8:01 + 2.minutes, "value2")
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
          .find(
            _.expirationTime.isAfter(now)
          )
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

object App6 extends helpers.ZIOAppDebug:
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
  // Result: Calls made: 141


import nl.vroste.rezilience.{
  CircuitBreaker,
  TrippingStrategy,
  Retry
}

val makeCircuitBreaker =
  CircuitBreaker.make(
    trippingStrategy =
      TrippingStrategy
        .failureCount(maxFailures =
          2
        ),
    resetPolicy =
      Retry.Schedules.common()
  )

object App7 extends helpers.ZIOAppDebug:
  import CircuitBreaker.CircuitBreakerOpen
  
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
  // Result: Calls prevented: 75 Calls made: 66


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

object App8 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val contractBreaches =
        Ref.make(0).run
  
      val makeRequest =
        defer:
          val hedged =
            logicThatSporadicallyLocksUp.race:
              logicThatSporadicallyLocksUp
                .delay:
                  25.millis
  
          val duration =
            hedged.run
          if (duration > 1.second)
            contractBreaches.update(_ + 1).run
  
      // TODO: explain the reason for silly
      // List of ()
      //       talk about how it'd be nice to have a
      //       ZIO operator for repeatNPar
      //       happy birthday bill
      ZIO
        .foreachPar(List.fill(50_000)(())):
          _ => makeRequest
        .run
  
      contractBreaches
        .get
        .debug("Contract Breaches")
        .run
  // Contract Breaches: 0
  // Result: 0


var attempts =
  0

def spottyLogic =
  defer:
    ZIO
      .attempt {
        attempts =
          attempts + 1
      }
      .run
    if (ZIO.attempt(attempts).run > 1)
      Random.nextIntBounded(3).run match
        case 0 =>
          Console.printLine("Success!").run
          ZIO.succeed(1).run
        case _ =>
          Console.printLine("Failed!").run
          ZIO.fail("Failed").run
    else
      Console.printLine("Failed!").run
      ZIO.fail("Failed").run