package Chapter03_Superpowers

import zio.*
import zio.direct.*

import Scenario.*

var currentScenario: Scenario = NeverWorks

enum Scenario:
  case Successful
  case NeverWorks
  case Slow
  case WorksOnTryInner(
      attempts: Int,
      ref: Ref[Int],
  )

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
end Scenario

object Scenario:
  // A bit of trickery here, so that the
  // reader thinks they're seeing
  // just-another-enum case, even though it's
  // calling some unsafe stuff behind the
  // scenes to create the *real* enum case

  // TODO Do we actually need to parameterize
  // this?
  //    We only ever pass 2 in
  def WorksOnTry(
      attempts: Int
  ): WorksOnTryInner =
    Unsafe.unsafe:
      implicit unsafe =>
        WorksOnTryInner(
          attempts,
          Runtime
            .default
            .unsafe
            .run(Ref.make(0))
            .getOrThrow(),
        )
end Scenario

def saveUser(username: String) =
  val succeed =
    ZIO.succeed:
      "User saved"

  val fail =
    ZIO
      .fail:
        "**Database crashed!!**"
      .tapError:
        error =>
          ZIO.debug:
            "Log: " + error

  val logic =
    defer:
      val scenario =
        ZIO.service[Scenario].run
      ZIO
        .debug("Attempting to save user")
        .run
      scenario match
        case Scenario.NeverWorks =>
          fail.run

        case Scenario.Slow =>
          ZIO.sleep(1.minute).run
          succeed.run

        case Scenario.WorksOnTryInner(
              attempts,
              ref,
            ) =>
          val numCalls =
            ref.getAndUpdate(_ + 1).run
          if numCalls == attempts then
            succeed.run
          else
            fail.run

        case Scenario.Successful =>
          succeed.run
      end match
  logic
end saveUser

def sendToManualQueue(username: String) =
  ZIO.attempt:
    s"Sent $username to manual queue"

def logUserSignup(username: String) =
  ZIO.debug:
    s"Log: Signup initiated for $userName"

val userName = "Morty"

val effect0 =
  saveUser:
    userName

import zio.*

object MyApp extends ZIOAppDefault:
  def run =
    Successful.simulate:
      effect0

object App0 extends helpers.ZIOAppDebug:
  def run =
    Successful.simulate:
      effect0
  // Attempting to save user
  // Result: User saved


object App1 extends helpers.ZIOAppDebug:
  def run =
    WorksOnTry(2).simulate:
      effect0
  // Attempting to save user
  // Log: **Database crashed!!**
  // Error: **Database crashed!!**


val effect1 = effect0.retryN(2)

object App2 extends helpers.ZIOAppDebug:
  def run =
    WorksOnTry(2).simulate:
      effect1
  // Attempting to save user
  // Log: **Database crashed!!**
  // Attempting to save user
  // Log: **Database crashed!!**
  // Attempting to save user
  // Result: User saved


object App3 extends helpers.ZIOAppDebug:
  def run =
    NeverWorks.simulate:
      effect1
  // Attempting to save user
  // Log: **Database crashed!!**
  // Attempting to save user
  // Log: **Database crashed!!**
  // Attempting to save user
  // Log: **Database crashed!!**
  // Error: **Database crashed!!**


val effect2 =
  effect1.orElseFail:
    "FAILURE: User not saved"

object App4 extends helpers.ZIOAppDebug:
  def run =
    NeverWorks.simulate:
      effect2
  // Attempting to save user
  // Log: **Database crashed!!**
  // Attempting to save user
  // Log: **Database crashed!!**
  // Attempting to save user
  // Log: **Database crashed!!**
  // Error: FAILURE: User not saved


val effect3 =
  effect2
    .timeoutFail("** Save timed out **"):
      5.seconds

object App5 extends helpers.ZIOAppDebug:
  def run =
    Slow.simulate:
      effect3
  // Attempting to save user
  // Error: ** Save timed out **


val effect4 =
  effect3.orElse:
    sendToManualQueue:
      userName

object App6 extends helpers.ZIOAppDebug:
  def run =
    NeverWorks.simulate:
      effect4
  // Attempting to save user
  // Log: **Database crashed!!**
  // Attempting to save user
  // Log: **Database crashed!!**
  // Attempting to save user
  // Log: **Database crashed!!**
  // Result: Sent Morty to manual queue


val effect5 =
  effect4.withFinalizer:
    username => logUserSignup(username)

object App7 extends helpers.ZIOAppDebug:
  def run =
    Successful.simulate:
      effect5
  // Attempting to save user
  // Log: Signup initiated for Morty
  // Result: User saved


val effect6 = effect5.timed

object App8 extends helpers.ZIOAppDebug:
  def run =
    Successful.simulate:
      effect6
  // Attempting to save user
  // Log: Signup initiated for Morty
  // Result: (PT0.000951929S,User saved)


val effect7 =
  effect6.when(userName != "Morty")

object App9 extends helpers.ZIOAppDebug:
  def run =
    Successful.simulate:
      effect7
  // Result: None


object App10 extends helpers.ZIOAppDebug:
  def run =
    Successful.simulate:
      ZIO.debug("Before save")
      effect1
  // Attempting to save user
  // Result: User saved


val effect8 =
  defer:
    ZIO.debug("Before save").run
    effect1.run

// TODO does the `def run`) force a new line in 
// other contexts? Or just my phone ebook reader?

object App11 extends helpers.ZIOAppDebug:
  def run =
    Successful.simulate:
      effect8
  // Before save
  // Attempting to save user
  // Result: User saved


object App12 extends helpers.ZIOAppDebug:
  def run =
    Successful.simulate:
      defer:
        ZIO.debug("**Before**").run
        effect8.debug.repeatN(1).run
        ZIO.debug("**After**").run
  // **Before**
  // Before save
  // Attempting to save user
  // User saved
  // Before save
  // Attempting to save user
  // User saved
  // **After**
