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

val errorScenarioConfig: Config[Option[ErrorsScenario]] =
  Config.Optional[ErrorsScenario](Config.fail("no default scenario"))

class ErrorsStaticConfigProvider(scenario: ErrorsScenario) extends ConfigProvider:
  override def load[A](config: Config[A])(implicit trace: Trace): IO[Config.Error, A] =
    ZIO.succeed(Some(scenario).asInstanceOf[A])

val errorsHappyPath =
  Runtime.setConfigProvider(ErrorsStaticConfigProvider(ErrorsScenario.HappyPath))

val errorsNetworkError =
  Runtime.setConfigProvider(ErrorsStaticConfigProvider(ErrorsScenario.NetworkError))
  
val errorsGpsError =
  Runtime.setConfigProvider(ErrorsStaticConfigProvider(ErrorsScenario.GPSError))

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

def temperatureCatchingApp(): String =
    try
      render:
        getTemperatureOrThrow()
    catch
      case ex: Exception =>
        "Failure"

def temperatureCatchingMoreApp(): String =
  try
    render:
      getTemperatureOrThrow()
  catch
    case ex: NetworkException =>
      "Network Unavailable"
    case ex: GpsFail =>
      "GPS Hardware Failure"

// TODO We hide the original implementation of this function, but show this one.
// Is that a problem? Seems unbalanced
val getTemperature: ZIO[Any, GpsFail | NetworkException, String] =
  defer:
    val maybeScenario = ZIO.config(errorScenarioConfig).orDie.run
    maybeScenario.getOrElse(ErrorsScenario.HappyPath) match
      case ErrorsScenario.GPSError =>
        ZIO.fail:
          GpsFail()
        .run
      case ErrorsScenario.NetworkError =>
        // TODO Use a non-exceptional error
        ZIO.fail:
          NetworkException()
        .run
      case ErrorsScenario.HappyPath =>
        ZIO.succeed:
          "Temperature: 35 degrees"
        .run

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

object Example05_Errors_0 extends ZIOAppDefault:
  def run =
    ZIO.attempt:
      temperatureApp()
  // Result: Success(Temperature: 35 degrees)


object Example05_Errors_1 extends ZIOAppDefault:
  scenario = ErrorsScenario.NetworkError
    
  def run =
    ZIO.succeed:
      temperatureApp()
  // Result: Success(Defect: NetworkException)


object Example05_Errors_2 extends ZIOAppDefault:
  scenario = ErrorsScenario.NetworkError
  
  def run =
    ZIO.succeed:
      temperatureCatchingApp()
  // Result: Success(Failure)


object Example05_Errors_3 extends ZIOAppDefault:
  scenario = ErrorsScenario.NetworkError
  
  def run =
    ZIO.succeed:
      temperatureCatchingMoreApp()
  // Result: Success(Network Unavailable)


object Example05_Errors_4 extends ZIOAppDefault:
  scenario = ErrorsScenario.GPSError
  
  def run =
    ZIO.succeed:
      temperatureCatchingMoreApp()
  // Result: Success(GPS Hardware Failure)


object Example05_Errors_5 extends ZIOAppDefault:
  override val bootstrap =
    errorsHappyPath
  
  def run =
    getTemperature
  // Result: Success(Temperature: 35 degrees)


object Example05_Errors_6 extends ZIOAppDefault:
  override val bootstrap =
    errorsNetworkError
  
  def run =
    getTemperature
  // Result: Success(repl.MdocSession$MdocApp$NetworkException)


object Example05_Errors_7 extends ZIOAppDefault:
  override val bootstrap =
    errorsGpsError
  
  def run =
    temperatureAppComplete
  // Result: Success(GPS Hardware Failure)


object Example05_Errors_8 extends ZIOAppDefault:
  scenario = ErrorsScenario.HappyPath
  
  def run =
    displayTemperatureZWrapped
  // Result: Success(35 degrees)


object Example05_Errors_9 extends ZIOAppDefault:
  scenario = ErrorsScenario.NetworkError
  
  def run =
    displayTemperatureZWrapped
  // Result: Success(Network Unavailable)


object Example05_Errors_10 extends ZIOAppDefault:
  scenario = ErrorsScenario.GPSError
  
  def run =
    getTemperatureWrapped.catchAll:
      case ex: NetworkException =>
        ZIO.succeed:
          "Network Unavailable"
  // Result: Success(Defect: GpsFail)


object Example05_Errors_11 extends ZIOAppDefault:
  scenario = ErrorsScenario.GPSError
  
  def run =
    getTemperatureWrapped.catchAll:
      case ex: NetworkException =>
        ZIO.succeed:
          "Network Unavailable"
      case other =>
        ZIO.succeed:
          "Unknown Error"
  // Result: Success(Unknown Error)
