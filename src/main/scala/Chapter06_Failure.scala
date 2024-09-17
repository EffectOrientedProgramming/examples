package Chapter06_Failure

import zio.*
import zio.direct.*

def divide(a: Int, b: Int): Int =
  a / b

case object FailObject

class FailException extends Exception:
  override def toString: String =
    "FailException"

def failureTypes(n: Int) =
  n match
    case 0 =>
      ZIO.fail("String fail")
    case 1 =>
      ZIO.fail(FailObject)
    case _ =>
      ZIO.fail(FailException())

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


def limitFail(n: Int, limit: Int) =
  defer:
    ZIO.debug(s"Executing step $n").run
    if n < limit then
      ZIO.succeed(s"Completed step $n").run
    else
      ZIO.fail(s"Failed at step $n").run

def shortCircuit(limit: Int) =
  defer:
    limitFail(0, limit).run
    limitFail(1, limit).run
    limitFail(2, limit).run

object App1 extends helpers.ZIOAppDebug:
  def run =
    shortCircuit(0).flip
  // Executing step 0
  // Result: Failed at step 0


object App2 extends helpers.ZIOAppDebug:
  def run =
    shortCircuit(1).flip
  // Executing step 0
  // Executing step 1
  // Result: Failed at step 1


object App3 extends helpers.ZIOAppDebug:
  def run =
    shortCircuit(2).flip
  // Executing step 0
  // Executing step 1
  // Executing step 2
  // Result: Failed at step 2


object App4 extends helpers.ZIOAppDebug:
  def run =
    shortCircuit(3)
  // Executing step 0
  // Executing step 1
  // Executing step 2
  // Result: Completed step 2


import Scenario.*

var currentScenario: Scenario = GPSFailure

enum Scenario:
  case Successful,
    TooCold,
    NetworkFailure,
    GPSFailure

  def simulate[E, A](
      effect: ZIO[Scenario & Scope, E, A]
  ) =
    defer:
      ZIO
        .succeed:
          currentScenario = this
        .run
      ZIO
        .scoped(effect)
        .provide(ZLayer.succeed(this))
        .run

class GpsException
    extends Exception("GPS Failure")
class NetworkException
    extends Exception("Network Failure")

case class Temperature(degrees: Int)

val getTemperature: ZIO[
  Scenario,
  GpsException | NetworkException,
  Temperature,
] =
  defer:
    val scenario = ZIO.service[Scenario].run
    ZIO.debug("Getting Temperature").run

    scenario match
      case Scenario.GPSFailure =>
        ZIO
          .fail:
            GpsException()
          .run
      case Scenario.NetworkFailure =>
        ZIO
          .fail:
            NetworkException()
          .run
      case Scenario.TooCold =>
        ZIO
          .succeed:
            Temperature(-20)
          .run
      case Scenario.Successful =>
        ZIO
          .succeed:
            Temperature(35)
          .run
    end match

object App5 extends helpers.ZIOAppDebug:
  def run =
    Successful.simulate:
      getTemperature
  // Getting Temperature
  // Result: Temperature(35)


object App6 extends helpers.ZIOAppDebug:
  def run =
    NetworkFailure.simulate:
      getTemperature
  // Getting Temperature
  // Defect: NetworkException: Network Failure


object App7 extends helpers.ZIOAppDebug:
  def run =
    NetworkFailure.simulate:
      defer:
        getTemperature.run
        ZIO.debug("succeeded").run
  // Getting Temperature
  // Defect: NetworkException: Network Failure


object App8 extends helpers.ZIOAppDebug:
  val displayTemperature =
    getTemperature.catchAll:
      case e: Exception =>
        ZIO.succeed("getTemperature failed")
  
  def run =
    NetworkFailure.simulate:
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

object App9 extends helpers.ZIOAppDebug:
  def run =
    GPSFailure.simulate:
      defer:
        val result = temperatureAppComplete.run
        ZIO.debug(result).run
  // Getting Temperature
  // GPS Hardware Failure


// Note - the error below does not get properly replaced when building on windows
val x = 0

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

object App10 extends helpers.ZIOAppDebug:
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

object App11 extends helpers.ZIOAppDebug:
  def run =
    TooCold.simulate:
      weatherReport
  // Getting Temperature
  // Checking Temperature
  // **Too Cold**


def getTemperatureOrThrow(): String =
  currentScenario match
    case Scenario.GPSFailure =>
      throw GpsException()
    case Scenario.NetworkFailure =>
      throw NetworkException()
    case _ =>
      "35 degrees"

object App12 extends helpers.ZIOAppDebug:
  def run =
    NetworkFailure.simulate:
      ZIO.succeed:
        getTemperatureOrThrow()
  // Defect: NetworkException: Network Failure


def safeTemperatureApp =
  ZIO.attempt:
    getTemperatureOrThrow()

object App13 extends helpers.ZIOAppDebug:
  def run =
    NetworkFailure.simulate:
      safeTemperatureApp.orElse:
        ZIO.succeed:
          "Could not get temperature"
  // Result: Could not get temperature
