package Chapter05_Managing_Failure

import zio.*
import zio.direct.*

enum Scenario:
  case HappyPath,
    Weird,
    NetworkFailure,
    GPSFailure

class GpsException          extends Exception("GPS Failure")
class NetworkException extends Exception("Network Failure")

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

def networkFailure =
  scenarioForNonZio = Some(Scenario.NetworkFailure)

  Runtime.setConfigProvider(
    ErrorsStaticConfigProvider(
      Scenario.NetworkFailure
    )
  )

def gpsFailure =
  scenarioForNonZio = Some(Scenario.GPSFailure)

  Runtime.setConfigProvider(
    ErrorsStaticConfigProvider(
      Scenario.GPSFailure
    )
  )

def weird =
  scenarioForNonZio = Some(Scenario.Weird)

  Runtime.setConfigProvider(
    ErrorsStaticConfigProvider(
      Scenario.Weird
    )
  )


val getTemperature: ZIO[
  Any,
  GpsException | NetworkException,
  String
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
      case Some(Scenario.Weird) =>
        ZIO
           .succeed:
             "Temperature: 34 degrees"
           .run
      case _ =>
         ZIO
           .succeed:
             "Temperature: 35 degrees"
           .run

object App0 extends helpers.ZIOAppDebug:
  override val bootstrap = happyPath
  
  def run =
    getTemperature
  // Result: Temperature: 35 degrees


object App1 extends helpers.ZIOAppDebug:
  override val bootstrap = networkFailure
  
  def run =
    getTemperature
  // Result: Defect: NetworkException: Network Failure


object App2 extends helpers.ZIOAppDebug:
  override val bootstrap = networkFailure
  
  def run =
    defer:
      getTemperature.run
      Console.printLine("will not print if getTemperature fails").run
  // Result: Defect: NetworkException: Network Failure


object App3 extends helpers.ZIOAppDebug:
  override val bootstrap = networkFailure
  
  def run =
    val safeGetTemperature =
      getTemperature.catchAll:
        case e: Exception =>
          ZIO.succeed("Could not get temperature")
  
    defer:
      safeGetTemperature.run
      Console.printLine("will not print if getTemperature fails").run
  // will not print if getTemperature fails


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
    case ex: GpsException =>
      ZIO.succeed:
        "GPS Hardware Failure"

object App4 extends helpers.ZIOAppDebug:
  override val bootstrap = gpsFailure
  
  def run =
    defer:
      val result =
        temperatureAppComplete.run
      Console.printLine(s"Didn't fail, despite: $result").run
  // Didn't fail, despite: GPS Hardware Failure


val getTemperatureBad =
  getTemperature.catchAll:
    case e: Exception =>
      ZIO.fail:
        e.getMessage

object App5 extends helpers.ZIOAppDebug:
  override val bootstrap = gpsFailure
  
  def run =
    getTemperatureBad.catchAll:
      case s: String =>
        Console.printLine(s)
  // GPS Failure


case class LocalizeFailure(s: String)

def localize(temperature: String) =
  if temperature.contains("35") then
    ZIO.succeed("Brrrr")
  else
    ZIO.fail:
      LocalizeFailure("I dunno")

// can fail with an Exception or a LocalizeFailure
val getTemperatureLocal =
  defer:
    // can fail with an Exception
    val temperature =
      getTemperature.run

    // can fail with a LocalizeFailure
    localize(temperature).run

object App6 extends helpers.ZIOAppDebug:
  override val bootstrap = weird
  
  def run =
    getTemperatureLocal.catchAll:
      case e: Exception =>
        Console.printLine(e.getMessage)
      case LocalizeFailure(s: String) =>
        Console.printLine(s)
  // I dunno




// since this function isn't a ZIO, it has to get the scenario from a var which is set when the bootstrap is set
def getTemperatureOrThrow(): String =
  scenarioForNonZio match
    case Some(Scenario.GPSFailure) =>
      throw GpsException()
    case Some(Scenario.NetworkFailure) =>
      throw NetworkException()
    case _ =>
      "35 degrees"

def render(value: String) =
  s"Temperature: $value"

def temperatureApp(): String =
  render:
    getTemperatureOrThrow()

object App7 extends helpers.ZIOAppDebug:
  def run =
    ZIO.attempt:
      temperatureApp()
  // Result: Temperature: 35 degrees


object App8 extends helpers.ZIOAppDebug:
  override val bootstrap = networkFailure
  
  def run =
    ZIO.succeed:
      temperatureApp()
  // Result: Defect: NetworkException: Network Failure


def temperatureCatchingApp(): String =
  try
    render:
      getTemperatureOrThrow()
  catch
    case ex: Exception =>
      "Failure"

object App9 extends helpers.ZIOAppDebug:
  override val bootstrap = networkFailure
  
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
    case ex: GpsException =>
      "GPS Hardware Failure"

object App10 extends helpers.ZIOAppDebug:
  override val bootstrap = networkFailure
  
  def run =
    ZIO.succeed:
      temperatureCatchingMoreApp()
  // Result: Network Unavailable


object App11 extends helpers.ZIOAppDebug:
  override val bootstrap = gpsFailure
  
  def run =
    ZIO.succeed:
      temperatureCatchingMoreApp()
  // Result: GPS Hardware Failure


val getTemperatureWrapped =
  ZIO.attempt:
    getTemperatureOrThrow()

val displayTemperatureZWrapped =
  getTemperatureWrapped.catchAll:
    case ex: NetworkException =>
      ZIO.succeed:
        "Network Unavailable"
    case ex: GpsException =>
      ZIO.succeed:
        "GPS problem"

object App12 extends helpers.ZIOAppDebug:
  override val bootstrap = happyPath
  
  def run =
    displayTemperatureZWrapped
  // Result: 35 degrees


object App13 extends helpers.ZIOAppDebug:
  override val bootstrap = networkFailure
  
  def run =
    displayTemperatureZWrapped
  // Result: Network Unavailable


object App14 extends helpers.ZIOAppDebug:
  override val bootstrap = gpsFailure
  
  def run =
    getTemperatureWrapped.catchAll:
      case ex: NetworkException =>
        ZIO.succeed:
          "Network Unavailable"
  // Result: Defect: GpsException


object App15 extends helpers.ZIOAppDebug:
  override val bootstrap = gpsFailure
  
  def run =
    getTemperatureWrapped.catchAll:
      case ex: NetworkException =>
        ZIO.succeed:
          "Network Unavailable"
      case other =>
        ZIO.succeed:
          "Unknown Error"
  // Result: Unknown Error
