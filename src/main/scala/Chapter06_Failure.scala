package Chapter06_Failure

import zio.*
import zio.direct.*

case object FailObject

object FailException extends Exception:
  override def toString: String =
    "FailException"

def failureTypes(n: Int) =
  n match
    case 0 =>
      ZIO.fail("String fail")
    case 1 =>
      ZIO.fail(FailObject)
    case _ =>
      ZIO.fail(FailException)

object App0 extends helpers.ZIOAppDebug:
  def run =
    defer:
      ZIO.debug("Begin failing").run
      failureTypes(0).flip.debug.run
      failureTypes(1).flip.debug.run
      failureTypes(2).flip.debug.run
      ZIO.debug("Done failing").run
  // Begin failing
  // String fail
  // FailObject
  // FailException
  // Done failing


def testLimit(n: Int, limit: Int) =
  defer:
    ZIO.debug(s"testLimit($n)").run
    if n < limit then
      ZIO.succeed(s"Passed $n").run
    else
      ZIO
        .debug(
          "n >= limit: testLimit failed"
        )
        .run
      ZIO.fail(s"Failed at $n").run

def shortCircuit(lim: Int) =
  defer:
    ZIO
      .debug(
        s"\n-- shortCircuit(limit: $lim) --"
      )
      .run
    testLimit(0, lim).run
    testLimit(1, lim).run
    testLimit(2, lim).run

object App1 extends helpers.ZIOAppDebug:
  def run =
    defer:
      shortCircuit(0).flip.run
      shortCircuit(1).flip.run
      shortCircuit(2).flip.run
      shortCircuit(3).run // Succeeds
  // 
  // -- shortCircuit(limit: 0) --
  // testLimit(0)
  // n >= limit: testLimit failed
  // 
  // -- shortCircuit(limit: 1) --
  // testLimit(0)
  // testLimit(1)
  // n >= limit: testLimit failed
  // 
  // -- shortCircuit(limit: 2) --
  // testLimit(0)
  // testLimit(1)
  // testLimit(2)
  // n >= limit: testLimit failed
  // 
  // -- shortCircuit(limit: 3) --
  // testLimit(0)
  // testLimit(1)
  // testLimit(2)
  // Result: Passed 2


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
      ZIO
        .config(scenarioConfig)
        .orDie
        .run // Invisible, okay usage of orDie
    ZIO.debug("Getting Temperature").run

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
  
  def run =
    getTemperature
  // Getting Temperature
  // Defect: NetworkException: Network Failure


object App4 extends helpers.ZIOAppDebug:
  override val bootstrap = networkFailure
  
  def run =
    defer:
      getTemperature.run
      ZIO.debug("getTemperature succeeded").run
  // Getting Temperature
  // Defect: NetworkException: Network Failure


object App5 extends helpers.ZIOAppDebug:
  override val bootstrap = networkFailure
  
  val displayTemperature =
    getTemperature.catchAll:
      case e: Exception =>
        ZIO.succeed("getTemperature failed")
  
  def run =
    defer:
      val result = displayTemperature.run
      ZIO.debug(result).run
  // Getting Temperature
  // getTemperature failed


val notExhaustive =
  getTemperature.catchAll:
    case ex: NetworkException =>
      ZIO.succeed:
        "Network Unavailable"

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
      ZIO.debug(result).run
  // Getting Temperature
  // GPS Hardware Failure


def check(t: Temperature) =
  defer:
    ZIO.debug("Checking Temperature").run
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

case class ClimateFailure(message: String)

object App7 extends helpers.ZIOAppDebug:
  def run =
    check(Temperature(-20))
  // Checking Temperature
  // Error: ClimateFailure(**Too Cold**)


val weatherReportFaulty =
  defer:
    val result = getTemperature.run
    check(result).run

val weatherReport =
  weatherReportFaulty.catchAll:
    case exception: Exception =>
      ZIO.debug(exception.getMessage)
    case failure: ClimateFailure =>
      ZIO.debug(failure.message)

object App8 extends helpers.ZIOAppDebug:
  override val bootstrap = tooCold
  
  def run =
    weatherReport
  // Getting Temperature
  // Checking Temperature
  // **Too Cold**


// since this function isn't a ZIO, it has to get the scenario from a var which is set when the bootstrap is set
def getTemperatureOrThrow: String =
  scenarioForNonZio match
    case Some(Scenario.GPSFailure) =>
      throw GpsException()
    case Some(Scenario.NetworkFailure) =>
      throw NetworkException()
    case _ =>
      "35 degrees"

object App9 extends helpers.ZIOAppDebug:
  override val bootstrap = networkFailure
  
  def run =
    ZIO.succeed:
      getTemperatureOrThrow
  // Defect: NetworkException: Network Failure


val safeTemperatureApp =
  ZIO.attempt:
    getTemperatureOrThrow

object App10 extends helpers.ZIOAppDebug:
  override val bootstrap = networkFailure
  
  def run =
    safeTemperatureApp.orElse:
      ZIO.succeed:
        "Could not get temperature"
  // Result: Could not get temperature
