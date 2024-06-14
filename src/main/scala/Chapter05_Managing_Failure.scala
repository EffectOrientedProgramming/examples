package Chapter05_Managing_Failure

import zio.*
import zio.direct.*

enum Scenario:
  case HappyPath,
    NetworkError,
    GPSError

class GpsFail          extends Exception
class NetworkException extends Exception

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
    ZIO.succeed(Some(scenario).asInstanceOf[A])

var scenarioForNonZio: Option[Scenario] = None

def happyPath =
  scenarioForNonZio = Some(Scenario.HappyPath)

  Runtime.setConfigProvider(
    ErrorsStaticConfigProvider(
      Scenario.HappyPath
    )
  )

def networkError =
  scenarioForNonZio = Some(Scenario.NetworkError)

  Runtime.setConfigProvider(
    ErrorsStaticConfigProvider(
      Scenario.NetworkError
    )
  )

def gpsError =
  scenarioForNonZio = Some(Scenario.GPSError)

  Runtime.setConfigProvider(
    ErrorsStaticConfigProvider(
      Scenario.GPSError
    )
  )

// since this function isn't a ZIO, it has to get the scenario from a var which is set when the bootstrap is set
def getTemperatureOrThrow(): String =
  scenarioForNonZio match
    case Some(Scenario.GPSError) =>
      throw GpsFail()
    case Some(Scenario.NetworkError) =>
      throw NetworkException()
    case _ =>
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
  override val bootstrap = networkError
  
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
  override val bootstrap = networkError
  
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
  override val bootstrap = networkError
  
  def run =
    ZIO.succeed:
      temperatureCatchingMoreApp()
  // Result: Network Unavailable


object App4 extends helpers.ZIOAppDebug:
  override val bootstrap = gpsError
  
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
      ZIO.config(scenarioConfig).orDie.run
      
    maybeScenario match
      case Some(Scenario.GPSError) =>
        ZIO
          .fail:
            GpsFail()
          .run
      case Some(Scenario.NetworkError) =>
        ZIO
          .fail:
            NetworkException()
          .run
      case _ =>
         ZIO
           .succeed:
             "Temperature: 35 degrees"
           .run

object App5 extends helpers.ZIOAppDebug:
  override val bootstrap = happyPath
  
  def run =
    getTemperature
  // Result: Temperature: 35 degrees


object App6 extends helpers.ZIOAppDebug:
  override val bootstrap = networkError
  
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
  override val bootstrap = gpsError
  
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
  override val bootstrap = happyPath
  
  def run =
    displayTemperatureZWrapped
  // Result: 35 degrees


object App9 extends helpers.ZIOAppDebug:
  override val bootstrap = networkError
  
  def run =
    displayTemperatureZWrapped
  // Result: Network Unavailable


object App10 extends helpers.ZIOAppDebug:
  override val bootstrap = gpsError
  
  def run =
    getTemperatureWrapped.catchAll:
      case ex: NetworkException =>
        ZIO.succeed:
          "Network Unavailable"
  // Result: Defect: GpsFail


object App11 extends helpers.ZIOAppDebug:
  override val bootstrap = gpsError
  
  def run =
    getTemperatureWrapped.catchAll:
      case ex: NetworkException =>
        ZIO.succeed:
          "Network Unavailable"
      case other =>
        ZIO.succeed:
          "Unknown Error"
  // Result: Unknown Error
