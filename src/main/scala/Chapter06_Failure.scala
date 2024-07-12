package Chapter06_Failure

import zio.*
import zio.direct.*

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

def weird =
  scenarioForNonZio =
    Some(Scenario.TooCold)

  Runtime.setConfigProvider(
    ErrorsStaticConfigProvider(
      Scenario.TooCold
    )
  )

case class Temperature(degrees: Int)

val getTemperature: ZIO[
  Any,
  GpsException | NetworkException,
  Temperature
] =
  defer:
    val maybeScenario =
      ZIO.config(scenarioConfig).orDie.run

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

object App0 extends helpers.ZIOAppDebug:
  override val bootstrap =
    happyPath
  
  def run =
    getTemperature
  // Result: Temperature(35)


object App1 extends helpers.ZIOAppDebug:
  override val bootstrap =
    networkFailure
  
  def run =
    getTemperature
  // Result: Defect: NetworkException: Network Failure


object App2 extends helpers.ZIOAppDebug:
  import zio.Console._
  
  override val bootstrap =
    networkFailure
  
  def run =
    defer:
      getTemperature.run
      printLine:
        "only prints if getTemperature succeeds"
      .run
  // Result: Defect: NetworkException: Network Failure


object App3 extends helpers.ZIOAppDebug:
  import zio.Console._
  
  override val bootstrap =
    networkFailure
  
  def run =
    val safeGetTemperature =
      getTemperature.catchAll:
        case e: Exception =>
          ZIO.succeed:
            "Could not get temperature"
  
    defer:
      safeGetTemperature.run
      printLine:
        "will not print if getTemperature fails"
      .run
  // will not print if getTemperature fails


val bad =
  getTemperature.catchAll:
    case ex: NetworkException =>
      ZIO.succeed:
        "Network Unavailable"
// [E029] Pattern Match Exhaustivity Warning:
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

object App4 extends helpers.ZIOAppDebug:
  import zio.Console._
  
  override val bootstrap =
    gpsFailure
  
  def run =
    defer:
      val result =
        temperatureAppComplete.run
      printLine:
        s"Didn't fail, despite: $result"
      .run
  // Didn't fail, despite: GPS Hardware Failure


val getTemperatureBad =
  getTemperature.catchAll:
    case e: Exception =>
      ZIO.fail:
        e.getMessage

object App5 extends helpers.ZIOAppDebug:
  import zio.Console._
  
  override val bootstrap =
    gpsFailure
  
  def run =
    getTemperatureBad.catchAll:
      case s: String =>
        printLine(s)
  // GPS Failure


case class ClimateFailure(message: String)

def check(temperature: Temperature) =
  if temperature.degrees > 0 then
    ZIO.succeed:
      "Not too cold."
  else
    ZIO.fail:
      ClimateFailure("**Machine froze**")

// can fail with an Exception or a ClimateFailure
val getTemperatureWithCheck =
  defer:
    // can fail with an Exception
    val temperature =
      getTemperature.run

    // can fail with a ClimateFailure
    check(temperature).run

object App6 extends helpers.ZIOAppDebug:
  override val bootstrap =
    gpsFailure
  
  def run =
    getTemperatureWithCheck
  // Result: Defect: GpsException: GPS Failure


object App7 extends helpers.ZIOAppDebug:
  import zio.Console._
  
  override val bootstrap =
    weird
  
  def run =
    getTemperatureWithCheck.catchAll:
      case exception: Exception =>
        printLine:
          exception.getMessage
      case failure: ClimateFailure =>
        printLine:
          failure.message
  // **Machine froze**


// since this function isn't a ZIO, it has to get the scenario from a var which is set when the bootstrap is set
def getTemperatureOrThrow(): String =
  scenarioForNonZio match
    case Some(Scenario.GPSFailure) =>
      throw GpsException()
    case Some(Scenario.NetworkFailure) =>
      throw NetworkException()
    case _ =>
      "35 degrees"

object App8 extends helpers.ZIOAppDebug:
  override val bootstrap =
    networkFailure
  
  def run =
    ZIO.succeed:
      getTemperatureOrThrow()
  // Result: Defect: NetworkException: Network Failure


val safeTemperatureApp =
  ZIO.attempt:
    getTemperatureOrThrow()

object App9 extends helpers.ZIOAppDebug:
  override val bootstrap =
    networkFailure
  
  def run =
    safeTemperatureApp.orElse:
      ZIO.succeed:
        "Could not get temperature"
  // Result: Could not get temperature
