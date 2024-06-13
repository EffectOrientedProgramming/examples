package Chapter05_Managing_Failure

import zio.*
import zio.direct.*

enum ErrorsScenario:
  case HappyPath,
    NetworkError,
    GPSError

class GpsFail          extends Exception
class NetworkException extends Exception

var scenario =
  ErrorsScenario.HappyPath

val errorScenarioConfig
    : Config[Option[ErrorsScenario]] =
  Config.Optional[ErrorsScenario](
    Config.fail("no default scenario")
  )

class ErrorsStaticConfigProvider(
    scenario: ErrorsScenario
) extends ConfigProvider:
  override def load[A](config: Config[A])(
      implicit trace: Trace
  ): IO[Config.Error, A] =
    ZIO.succeed(Some(scenario).asInstanceOf[A])

object Scenario:

  val happyPath =
    Runtime.setConfigProvider(
      ErrorsStaticConfigProvider(
        ErrorsScenario.HappyPath
      )
    )

  val networkError =
    Runtime.setConfigProvider(
      ErrorsStaticConfigProvider(
        ErrorsScenario.NetworkError
      )
    )

  val gpsError =
    Runtime.setConfigProvider(
      ErrorsStaticConfigProvider(
        ErrorsScenario.GPSError
      )
    )

// TODO Hide definition? Then we won't see the internals of the scenario stuff.
// This would also makes the exceptions more surprising
def getTemperatureOrThrow(): String =
  scenario match
    case ErrorsScenario.GPSError =>
      throw GpsFail()
    case ErrorsScenario.NetworkError =>
      throw NetworkException()
    case ErrorsScenario.HappyPath =>
      "35 degrees"

def render(value: String) =
  s"Temperature: $value"

def temperatureApp(): String =
  render:
    getTemperatureOrThrow()

object App0 extends helpers.ZIOAppDebug:
  def run =
    ZIO.attempt:
      temperatureApp()
  // Result: Temperature: 35 degrees


object App1 extends helpers.ZIOAppDebug:
  scenario =
    ErrorsScenario.NetworkError
  
  def run =
    ZIO.succeed:
      temperatureApp()
  // Result: Defect: NetworkException


def temperatureCatchingApp(): String =
  try
    render:
      getTemperatureOrThrow()
  catch
    case ex: Exception =>
      "Failure"

object App2 extends helpers.ZIOAppDebug:
  scenario =
    ErrorsScenario.NetworkError
  
  def run =
    ZIO.succeed:
      temperatureCatchingApp()
  // Result: Failure


def temperatureCatchingMoreApp(): String =
  try
    render:
      getTemperatureOrThrow()
  catch
    case ex: NetworkException =>
      "Network Unavailable"
    case ex: GpsFail =>
      "GPS Hardware Failure"

object App3 extends helpers.ZIOAppDebug:
  scenario =
    ErrorsScenario.NetworkError
  
  def run =
    ZIO.succeed:
      temperatureCatchingMoreApp()
  // Result: Network Unavailable


object App4 extends helpers.ZIOAppDebug:
  scenario =
    ErrorsScenario.GPSError
  
  def run =
    ZIO.succeed:
      temperatureCatchingMoreApp()
  // Result: GPS Hardware Failure


// TODO We hide the original implementation of this function, but show this one.
// Is that a problem? Seems unbalanced
val getTemperature: ZIO[
  Any,
  GpsFail | NetworkException,
  String
] =
  defer:
    val maybeScenario =
      ZIO.config(errorScenarioConfig).orDie.run
    maybeScenario
      .getOrElse(ErrorsScenario.HappyPath) match
      case ErrorsScenario.GPSError =>
        ZIO
          .fail:
            GpsFail()
          .run
      case ErrorsScenario.NetworkError =>
        // TODO Use a non-exceptional error
        ZIO
          .fail:
            NetworkException()
          .run
      case ErrorsScenario.HappyPath =>
        ZIO
          .succeed:
            "Temperature: 35 degrees"
          .run

object App5 extends helpers.ZIOAppDebug:
  override val bootstrap =
    Scenario.happyPath
  
  def run =
    getTemperature
  // Result: Temperature: 35 degrees


object App6 extends helpers.ZIOAppDebug:
  override val bootstrap =
    Scenario.networkError
  
  def run =
    getTemperature
  // Result: repl.MdocSession$MdocApp$NetworkException


val bad =
  getTemperature.catchAll:
    case ex: NetworkException =>
      ZIO.succeed:
        "Network Unavailable"

val temperatureAppComplete =
  getTemperature.catchAll:
    case ex: NetworkException =>
      ZIO.succeed:
        "Network Unavailable"
    case ex: GpsFail =>
      ZIO.succeed:
        "GPS Hardware Failure"

object App7 extends helpers.ZIOAppDebug:
  override val bootstrap =
    Scenario.gpsError
  
  def run =
    temperatureAppComplete
  // Result: GPS Hardware Failure


val getTemperatureWrapped =
  ZIO.attempt:
    getTemperatureOrThrow()

val displayTemperatureZWrapped =
  getTemperatureWrapped.catchAll:
    case ex: NetworkException =>
      ZIO.succeed:
        "Network Unavailable"
    case ex: GpsFail =>
      ZIO.succeed:
        "GPS problem"

object App8 extends helpers.ZIOAppDebug:
  scenario =
    ErrorsScenario.HappyPath
  
  def run =
    displayTemperatureZWrapped
  // Result: 35 degrees


object App9 extends helpers.ZIOAppDebug:
  scenario =
    ErrorsScenario.NetworkError
  
  def run =
    displayTemperatureZWrapped
  // Result: Network Unavailable


object App10 extends helpers.ZIOAppDebug:
  scenario =
    ErrorsScenario.GPSError
  
  def run =
    getTemperatureWrapped.catchAll:
      case ex: NetworkException =>
        ZIO.succeed:
          "Network Unavailable"
  // Result: Defect: GpsFail


object App11 extends helpers.ZIOAppDebug:
  scenario =
    ErrorsScenario.GPSError
  
  def run =
    getTemperatureWrapped.catchAll:
      case ex: NetworkException =>
        ZIO.succeed:
          "Network Unavailable"
      case other =>
        ZIO.succeed:
          "Unknown Error"
  // Result: Unknown Error
