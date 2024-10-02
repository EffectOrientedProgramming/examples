package Chapter09_Resilience

import zio.*
import zio.direct.*

import zio.{ZIO, ZLayer}
// This utilizes: https://zio.dev/zio-cache/
// These types have good documentation and can be ctrl-click'ed into
import zio.cache.{Cache, Lookup}

import java.nio.file.{Path, Paths}

case class FSLive(requests: Ref[Int])
    extends CloudStorage:
  def retrieve(
      name: String
  ): ZIO[Any, Nothing, FileContents] =
    defer:
      requests.update(_ + 1).run
      ZIO.sleep(10.millis).run
      FSLive.hardcodedContents

  val invoice: ZIO[Any, Nothing, String] =
    defer:
      val count = requests.get.run
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
      name: String
  ): ZIO[Any, Nothing, FileContents]
  val invoice: ZIO[Any, Nothing, String]

object CloudStorage:
  val live =
    ZLayer.fromZIO:
      defer:
        FSLive(Ref.make(0).run)

case class PopularService(
    retrieveContents: String => ZIO[
      Any,
      Nothing,
      FileContents,
    ]
):
  def retrieve(name: String) =
    retrieveContents(name)

val thunderingHerds =
  defer:
    val popularService =
      ZIO.service[PopularService].run

    val memes =
      List.fill(100):
        popularService.retrieve:
          "Awesome Memes"

    ZIO.collectAllPar(memes).run

    ZIO
      .serviceWithZIO[CloudStorage]:
        storage => storage.invoice
      .run

val makePopularService =
  defer:
    val storage =
      ZIO.service[CloudStorage].run

    PopularService(storage.retrieve)

object App0 extends helpers.ZIOAppDebug:
  def run =
    thunderingHerds.provide(
      CloudStorage.live,
      ZLayer.fromZIO(makePopularService),
    )
  // Result: Amount owed: $100


import zio.cache.Cache

val makeCachedPopularService =
  defer:
    val storage =
      ZIO.service[CloudStorage].run

    val cache =
      Cache
        .make(
          capacity = 1,
          timeToLive = Duration.Infinity,
          lookup = Lookup(storage.retrieve),
        )
        .run

    PopularService(cache.get)

object App1 extends helpers.ZIOAppDebug:
  def run =
    thunderingHerds.provide(
      CloudStorage.live,
      ZLayer.fromZIO:
        makeCachedPopularService,
    )
  // Result: Amount owed: $1


import java.time.Instant

def expensiveCall(
    globalStart: Instant,
    caller: String,
) =
  defer:
    val now = Clock.instant.run

    val seconds =
      java
        .time
        .Duration
        .between(globalStart, now)
        .getSeconds

    println:
      s"$caller request @ ${seconds}s"

    ZIO.sleep(30.millis).run

object App2 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val startTime = Clock.instant.run
      expensiveCall(startTime, "User")
        .repeatN(2)
        .run
  // User request @ 0s
  // User request @ 0s
  // User request @ 0s


import nl.vroste.rezilience.RateLimiter

val makeRateLimiter =
  RateLimiter
    .make(max = 1, interval = 1.second)

object App3 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val startTime   = Clock.instant.run
      val rateLimiter = makeRateLimiter.run
      rateLimiter:
        expensiveCall(startTime, "User")
      .repeatN(2)
        .run
  // User request @ 0s
  // User request @ 1s
  // User request @ 2s


object App4 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val rateLimiter = makeRateLimiter.run
      val users =
        List("Bill ", "Bruce", "James")
      val startTime = Clock.instant.run
      ZIO
        .foreachPar(users):
          user =>
            rateLimiter:
              expensiveCall(startTime, user)
            .repeatN(2)
        .as("All requests succeeded")
        .run
  // Bruce request @ 0s
  // James request @ 1s
  // Bill  request @ 2s
  // Bruce request @ 3s
  // James request @ 4s
  // Bill  request @ 5s
  // Bruce request @ 6s
  // James request @ 7s
  // Bill  request @ 8s
  // Result: All requests succeeded


trait DelicateResource:
  val request: ZIO[Any, String, List[Char]]

case class Live(
    requestIdQueue: Queue[Char],
    currentRequestsRef: Ref.Synchronized[
      (Option[Char], List[Char])
    ],
) extends DelicateResource:
  val request =
    defer:
      val (
        thisRequestOption,
        currentRequests,
      ) =
        currentRequestsRef
          .updateAndGetZIO:
            (_, currentRequests) =>
              defer:
                // we do this here so we get
                // sequential request ids
                val res =
                  requestIdQueue.take.run

                if currentRequests.size > 3
                then
                  ZIO
                    .fail(
                      "Crashed the server!!"
                    )
                    .run
                else
                  val updatedRequests =
                    currentRequests :+ res
                  ZIO
                    .debug(
                      s"Current requests: $updatedRequests"
                    )
                    .run
                  (
                    Some(res),
                    updatedRequests,
                  )
          .run

      val thisRequest = thisRequestOption.get
      ZIO
        .sleep:
          thisRequest.intValue.millis * 10
        .run

      currentRequestsRef
        .update:
          (_, currentRequests) =>
            (
              thisRequestOption,
              currentRequests
                .filterNot(_ == thisRequest),
            )
        .run

      currentRequests
end Live

object DelicateResource:
  val live =
    ZLayer.fromZIO:
      defer:
        ZIO
          .debug:
            "Delicate Resource constructed."
          .run
        ZIO
          .debug:
            "Do not make more than 3 concurrent requests!"
          .run

        val atoz = 'A' to 'Z'
        val orderingQueue =
          Queue.unbounded[Char].run
        orderingQueue.offerAll(atoz).run

        val currentRequests =
          Ref
            .Synchronized
            .make(
              Option.empty[Char],
              List.empty[Char],
            )
            .run

        Live(orderingQueue, currentRequests)
end DelicateResource

object App5 extends helpers.ZIOAppDebug:
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
  // Current requests: List(A)
  // Current requests: List(A, B)
  // Current requests: List(A, B, C)
  // Current requests: List(A, B, C, D)
  // Error: Crashed the server!!


import nl.vroste.rezilience.Bulkhead

val makeBulkhead =
  Bulkhead.make(maxInFlightCalls = 3)

object App6 extends helpers.ZIOAppDebug:
  def run =
    ZIO
      .scoped:
        defer:
          val bulkhead = makeBulkhead.run
          val delicateResource =
            ZIO.service[DelicateResource].run
          ZIO
            .foreachPar(1 to 10):
              _ =>
                bulkhead:
                  delicateResource.request
            .as("All Requests Succeeded")
            .run
      .provide(DelicateResource.live)
  // Delicate Resource constructed.
  // Do not make more than 3 concurrent requests!
  // Current requests: List(A)
  // Current requests: List(A, B)
  // Current requests: List(A, B, C)
  // Current requests: List(B, C, D)
  // Current requests: List(C, D, E)
  // Current requests: List(D, E, F)
  // Current requests: List(E, F, G)
  // Current requests: List(F, G, H)
  // Current requests: List(G, H, I)
  // Current requests: List(H, I, J)
  // Result: All Requests Succeeded


import zio.Ref

import java.time.Instant
import scala.concurrent.TimeoutException

// Invisible mdoc fences
import zio.Runtime.default.unsafe

val timeSensitiveValue =
  Unsafe.unsafe(
    (u: Unsafe) =>
      given Unsafe =
        u
      unsafe
        .run(
          scheduledValues(
            (1_100.millis, true),
            (4_100.millis, false),
            (5_000.millis, true),
          )
        )
        .getOrThrowFiberFailure()
  )

def externalService(
    callsMade: Ref[Int],
    failures: Ref[Int],
) =
  defer:
    callsMade.update(_ + 1).run
    val b = timeSensitiveValue.run
    if b then
      ZIO.unit.run
    else
      failures.update(_ + 1).run
      ZIO.fail(()).run

object InstantOps:
  extension (i: Instant)
    def plusZ(
        duration: zio.Duration
    ): Instant =
      i.plus(duration.asJava)

import InstantOps.*

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
    A,
  ],
] =
  defer:
    val startTime = Clock.instant.run
    val timeTable =
      createTimeTableX(
        startTime,
        value,
        values* // Yay Scala3 :)
      )
    accessX(timeTable)

// make this function more obvious
private def createTimeTableX[A](
    startTime: Instant,
    value: (Duration, A),
    values: (Duration, A)*
): Seq[ExpiringValue[A]] =
  values.scanLeft(
    ExpiringValue(
      startTime.plusZ(value._1),
      value._2,
    )
  ):
    case (
          ExpiringValue(elapsed, _),
          (duration, value),
        ) =>
      ExpiringValue(
        elapsed.plusZ(duration),
        value,
      )

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
  defer:
    val now = Clock.instant.run
    ZIO
      .getOrFailWith(
        new TimeoutException("TOO LATE")
      ):
        timeTable
          .find(
            _.expirationTime.isAfter(now)
          )
          .map(_.value)
      .run

private case class ExpiringValue[A](
    expirationTime: Instant,
    value: A,
)

val rapidly =
  Schedule.recurs(140) &&
    Schedule.spaced(50.millis)

object App7 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val callsMade = Ref.make(0).run
      val failures  = Ref.make(0).run
      externalService(callsMade, failures)
        .ignore
        .repeat(rapidly)
        .run
  
      val made   = callsMade.get.run
      val failed = failures.get.run
  
      s"""
Total Submitted: $made
Failed: $failed
""".stripMargin
  // Result: 
  // Total Submitted: 141
  // Failed: 78


import nl.vroste.rezilience.{
  CircuitBreaker,
  TrippingStrategy,
  Retry,
}
import TrippingStrategy.failureCount

val circuitBreakerZ =
  CircuitBreaker.make(
    trippingStrategy = failureCount(2),
    resetPolicy = Retry.Schedules.common(),
  )

object App8 extends helpers.ZIOAppDebug:
  import CircuitBreaker.CircuitBreakerOpen
  
  def run =
    defer:
      val circuitBreaker = circuitBreakerZ.run
      val callsMade      = Ref.make[Int](0).run
      val callsPrevented = Ref.make[Int](0).run
      val failures       = Ref.make[Int](0).run
  
      val protectedCall =
        circuitBreaker:
          externalService(callsMade, failures)
        .catchSome:
          case CircuitBreakerOpen =>
            callsPrevented.update(_ + 1)
          case other =>
            ZIO.unit
  
      protectedCall.ignore.repeat(rapidly).run
  
      val prevented = callsPrevented.get.run
      val made      = callsMade.get.run
      val failed    = failures.get.run
      s"""
Total Submitted: $made
Failed: $failed
Prevented: $prevented
""".stripMargin
  // Result: 
  // Total Submitted: 62
  // Failed: 0
  // Prevented: 79


val intermittentSlowResponse =
  defer:
    if Random.nextIntBounded(1_000).run == 0
    then
      ZIO.sleep(3.second).run

def lotsOfRequests(
    totalRequests: Int,
    timeout: Duration,
    request: ZIO[Any, Throwable, Unit],
): ZIO[Any, Nothing, Int] =
  defer:
    val successes =
      ZIO
        // Evaluate and run each effect in
        // the structure in parallel, and
        // collect discarding failed ones.
        .collectAllSuccessesPar:
          List.fill(totalRequests):
            request
              .timeoutFail("took too long"):
                timeout
        .run

    totalRequests - successes.length

object App9 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val numFailed =
        lotsOfRequests(
          50_000,
          1.second,
          intermittentSlowResponse,
        ).run
      s"$numFailed requests timed out"
  // Result: 49 requests timed out


val hedged =
  intermittentSlowResponse.race:
    intermittentSlowResponse.delay:
      20.millis

object App10 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val numFailed =
        lotsOfRequests(
          50_000,
          1.second,
          hedged,
        ).run
  
      s"$numFailed requests timed out"
  // Result: 0 requests timed out


val attemptsR =
  Unsafe.unsafe:
    implicit unsafe =>
      Runtime
        .default
        .unsafe
        .run(Ref.make(0))
        .getOrThrowFiberFailure()

def spottyLogic =
  defer:
    val attemptsCur =
      attemptsR.getAndUpdate(_ + 1).run
    if ZIO.attempt(attemptsCur).run == 3 then
      ZIO.debug("Success!").run
      ZIO.succeed(true).run
    else
      ZIO.debug("Failed!").run
      ZIO.succeed(false).run