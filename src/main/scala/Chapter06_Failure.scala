package Chapter06_Failure

import zio.*
import zio.direct.*
import zio.Console.*

object ObjectX

object ExceptionX extends Exception:
  override def toString: String =
    "ExceptionX"

def failureTypes(n: Int) =
  n match
    case 0 =>
      ZIO.fail("String fail")
    case 1 =>
      ZIO.fail(ObjectX)
    case _ =>
      ZIO.fail(ExceptionX)

object App0 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val r0 = failureTypes(0).flip.run
      printLine(s"r0: $r0").run
      val r1 = failureTypes(1).flip.run
      printLine(s"r1: $r1").run
      val r2 = failureTypes(2).flip.run
      printLine(s"r2: $r2").run
  // r0: String fail
  // r1: repl.MdocSession$MdocApp$ObjectX$@1b0d72ac
  // r2: ExceptionX


def testLimit(n: Int, limit: Int) =
  println(s"testLimit($n, $limit)")
  if n < limit then
    ZIO.succeed(s"Passed $n")
  else
    println("n >= limit: testLimit failed")
    ZIO.fail(s"Failed at $n")

def shortCircuiting(n: Int) =
  defer:
    val r1 = testLimit(0, n).run
    printLine(s"-> n: $n, r1: $r1").run
    val r2 = testLimit(1, n).run
    printLine(s"-> n: $n, r2: $r2").run
    val r3 = testLimit(2, n).run
    printLine(s"-> n: $n, r3: $r3").run

object App1 extends helpers.ZIOAppDebug:
  def run =
    defer:
      val result0 = shortCircuiting(0).flip.run
      printLine(s"result0: $result0").run
      val result1 = shortCircuiting(1).flip.run
      printLine(s"result1: $result1").run
      val result2 = shortCircuiting(2).flip.run
      printLine(s"result2: $result2").run
  // testLimit(0, 0)
  // n >= limit: testLimit failed
  // result0: Failed at 0
  // testLimit(0, 1)
  // -> n: 1, r1: Passed 0
  // testLimit(1, 1)
  // n >= limit: testLimit failed
  // result1: Failed at 1
  // testLimit(0, 2)
  // -> n: 2, r1: Passed 0
  // testLimit(1, 2)
  // -> n: 2, r2: Passed 1
  // testLimit(2, 2)
  // n >= limit: testLimit failed
  // result2: Failed at 2


enum Scenario:
  case HappyPath,
    TooCold,
    NetworkFailure,
    GPSFailure

class GpsException
    extends Exception("GPS Failure")
class NetworkException
    extends Exception("Network Failure")

val scenarioConfig
    : Config[Option[Scenario]] =
  Config.Optional[Scenario](
    Config.fail("no default scenario")
  )

class ErrorsStaticConfigProvider(
    scenario: Scenario
) extends ConfigProvider:
  override def load[A](config: Config[A])(
      implicit trace: Trace
  ): IO[Config.Error, A] =
    ZIO.succeed(
      Some(scenario).asInstanceOf[A]
    )

var scenarioForNonZio: Option[Scenario] =
  None

def happyPath =
  scenarioForNonZio =
    Some(Scenario.HappyPath)

  Runtime.setConfigProvider(
    ErrorsStaticConfigProvider(
      Scenario.HappyPath
    )
  )

def networkFailure =
  scenarioForNonZio =
    Some(Scenario.NetworkFailure)

  Runtime.setConfigProvider(
    ErrorsStaticConfigProvider(
      Scenario.NetworkFailure
    )
  )

def gpsFailure =
  scenarioForNonZio =
    Some(Scenario.GPSFailure)

  Runtime.setConfigProvider(
    ErrorsStaticConfigProvider(
      Scenario.GPSFailure
    )
  )

def tooCold =
  scenarioForNonZio = Some(Scenario.TooCold)

  Runtime.setConfigProvider(
    ErrorsStaticConfigProvider(
      Scenario.TooCold
    )
  )

case class Temperature(degrees: Int)

val getTemperature: ZIO[
  Any,
  GpsException | NetworkException,
  Temperature,
] =
  defer:
    val maybeScenario =
      ZIO.config(scenarioConfig).orDie.run
    printLine("Getting Temperature")
      .orDie
      .run

    maybeScenario match
      case Some(Scenario.GPSFailure) =>
        ZIO
          .fail:
            GpsException()
          .run
      case Some(Scenario.NetworkFailure) =>
        ZIO
          .fail:
            NetworkException()
          .run
      case Some(Scenario.TooCold) =>
        ZIO
          .succeed:
            Temperature(-20)
          .run
      case _ =>
        ZIO
          .succeed:
            Temperature(35)
          .run
    end match

object App2 extends helpers.ZIOAppDebug:
  override val bootstrap = happyPath
  
  def run =
    getTemperature
  // Getting Temperature
  // Result: Temperature(35)


object App3 extends helpers.ZIOAppDebug:
  override val bootstrap = networkFailure
  
  // TODO Reduce output here
  
  def run =
    getTemperature
  // Getting Temperature
  // Result: Defect: NetworkException: Network Failure


object App4 extends helpers.ZIOAppDebug:
  override val bootstrap = networkFailure
  
  def run =
    defer:
      getTemperature.run
      printLine("getTemperature succeeded").run
  // Getting Temperature
  // Result: Defect: NetworkException: Network Failure


object App5 extends helpers.ZIOAppDebug:
  override val bootstrap = networkFailure
  
  def run =
    val safeGetTemperature =
      getTemperature.catchAll:
        case e: Exception =>
          ZIO.succeed:
            "Could not get temperature"
  
    defer:
      val result = safeGetTemperature.run
      printLine(result).run
  // Getting Temperature
  // Could not get temperature


val bad =
  getTemperature.catchAll:
    case ex: NetworkException =>
      ZIO.succeed:
        "Network Unavailable"
// Pattern Match Exhaustivity Warning:
//     case ex: NetworkException =>
//     ^
// match may not be exhaustive.
//
// It would fail on pattern case: _: GpsException

val temperatureAppComplete =
  getTemperature.catchAll:
    case ex: NetworkException =>
      ZIO.succeed:
        "Network Unavailable"
    case ex: GpsException =>
      ZIO.succeed:
        "GPS Hardware Failure"

object App6 extends helpers.ZIOAppDebug:
  override val bootstrap = gpsFailure
  
  def run =
    defer:
      val result = temperatureAppComplete.run
      printLine(result).run
  // Getting Temperature
  // GPS Hardware Failure


case class ClimateFailure(message: String)

def check(t: Temperature) =
  defer:
    printLine("Checking Temperature").run
    if t.degrees > 0 then
      ZIO
        .succeed:
          "Comfortable Temperature"
        .run
    else
      ZIO
        .fail:
          ClimateFailure("**Too Cold**")
        .run

object App7 extends helpers.ZIOAppDebug:
  def run =
    check(Temperature(-20))
  // Checking Temperature
  // Result: ClimateFailure(**Too Cold**)


object App8 extends helpers.ZIOAppDebug:
  def run =
    check(Temperature(15))
  // Checking Temperature
  // Result: Comfortable Temperature


// TODO Subhead name

val weatherReportFaulty =
  defer:
    val result = getTemperature.run
    check(result).run

val weatherReport =
  weatherReportFaulty.catchAll:
    case exception: Exception =>
      printLine(exception.getMessage)
    case failure: ClimateFailure =>
      printLine(failure.message)

object App9 extends helpers.ZIOAppDebug:
  override val bootstrap = tooCold
  
  def run =
    weatherReport
  // Getting Temperature
  // Checking Temperature
  // **Too Cold**


object App10 extends helpers.ZIOAppDebug:
  override val bootstrap = gpsFailure
  
  def run =
    weatherReport
  // Getting Temperature
  // GPS Failure


// since this function isn't a ZIO, it has to get the scenario from a var which is set when the bootstrap is set
def getTemperatureOrThrow: String =
  scenarioForNonZio match
    case Some(Scenario.GPSFailure) =>
      throw GpsException()
    case Some(Scenario.NetworkFailure) =>
      throw NetworkException()
    case _ =>
      "35 degrees"

object App11 extends helpers.ZIOAppDebug:
  override val bootstrap = networkFailure
  
  def run =
    ZIO.succeed:
      getTemperatureOrThrow
  // Result: Defect: NetworkException: Network Failure


val safeTemperatureApp =
  ZIO.attempt:
    getTemperatureOrThrow

object App12 extends helpers.ZIOAppDebug:
  override val bootstrap = networkFailure
  
  def run =
    safeTemperatureApp.orElse:
      ZIO.succeed:
        "Could not get temperature"
  // Result: Could not get temperature
