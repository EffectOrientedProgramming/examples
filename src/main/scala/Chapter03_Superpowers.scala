package Chapter03_Superpowers

import zio.*
import zio.direct.*

enum Scenario:
  case HappyPath
  case NeverWorks
  case Slow
  case WorksOnTry(attempts: Int, ref: Ref[Int])

// This configuration is used by effects to get the scenario that
// may have been passed in via `bootstrap`
// The configuration is optional and the default of `Config.fail`
// sets the Option to None.
val scenarioConfig: Config[Option[Scenario]] =
  Config.Optional[Scenario](
    Config.fail("no default scenario")
  )

class StaticConfigProvider(scenario: Scenario)
    extends ConfigProvider:
  override def load[A](config: Config[A])(
      implicit trace: Trace
  ): IO[Config.Error, A] =
    ZIO.succeed(Some(scenario).asInstanceOf[A])

val happyPath =
  Runtime.setConfigProvider:
    StaticConfigProvider(Scenario.HappyPath)

val neverWorks =
  Runtime.setConfigProvider:
    StaticConfigProvider(Scenario.NeverWorks)

val slow =
  Runtime.setConfigProvider:
    StaticConfigProvider(Scenario.Slow)

val doesNotWorkInitially =
  val scenario =
    Unsafe.unsafe:
      implicit unsafe =>
        Scenario.WorksOnTry(
          3,
          Runtime
            .default
            .unsafe
            .run(Ref.make(1))
            .getOrThrow()
        )
  Runtime.setConfigProvider:
    StaticConfigProvider(scenario)

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
          Console.printLine:
            "Log: " + error

  def saveForScenario(maybeScenario: Option[Scenario]) =
    defer:
      maybeScenario match
        case Some(Scenario.NeverWorks) =>
          fail.run

        case Some(Scenario.Slow) =>
          ZIO
            .sleep(1.minute)
            .onInterrupt:
              ZIO.debug:
                "Log: Interrupting slow request"
            .run
          succeed.run

        case Some(Scenario.WorksOnTry(attempts, ref)) =>
          val numCalls =
            ref.getAndUpdate(_ + 1).run
          if numCalls == attempts then
            succeed.run
          else
            fail.run

        case _ =>
          succeed.run

  defer:
    val maybeScenario =
      ZIO.config(scenarioConfig).run
    saveForScenario:
      maybeScenario
    .run
end saveUser

def sendToManualQueue(username: String) =
  ZIO.attempt:
    s"Please manually provision $username"

val logUserSignup =
  Console
    .printLine:
      s"Log: Signup initiated for $userName"
    .orDie

val userName =
  "Morty"

val effect0 =
  saveUser:
    userName

object MyApp extends ZIOAppDefault:
  def run =
    effect0

object App0 extends helpers.ZIOAppDebug:
  def run =
    effect0
  // Result: User saved


object App1 extends helpers.ZIOAppDebug:
  override val bootstrap =
    happyPath
  
  def run =
    effect0
  // Result: User saved


object App2 extends helpers.ZIOAppDebug:
  override val bootstrap =
    neverWorks
  
  def run =
    effect0
  // Log: **Database crashed!!**
  // Result: **Database crashed!!**


val effect1 =
  effect0.retryN(2)

object App3 extends helpers.ZIOAppDebug:
  override val bootstrap =
    doesNotWorkInitially
  
  def run =
    effect1
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Result: User saved


object App4 extends helpers.ZIOAppDebug:
  override val bootstrap =
    neverWorks
  
  def run =
    effect1
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Result: **Database crashed!!**


val effect2 =
  effect1.orElseFail:
    "ERROR: User could not be saved"

object App5 extends helpers.ZIOAppDebug:
  override val bootstrap =
    neverWorks
  
  def run =
    effect2
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Result: ERROR: User could not be saved


val effect3 =
  effect2.timeoutFail("*** Save timed out ***"):
    5.seconds

object App6 extends helpers.ZIOAppDebug:
  override val bootstrap =
    slow
  
  def run =
    effect3
  // Result: *** Save timed out ***


val effect4 =
  effect3.orElse:
    sendToManualQueue:
      userName

object App7 extends helpers.ZIOAppDebug:
  override val bootstrap =
    neverWorks
  
  def run =
    effect4
  // Log: **Database crashed!!**
  // Result: Please manually provision Morty


val effect5 =
  effect4.withFinalizer:
    _ => logUserSignup

object App8 extends helpers.ZIOAppDebug:
  override val bootstrap =
    happyPath
  
  def run =
    effect5
  // Result: User saved


val effect6 =
  effect5.timed

object App9 extends helpers.ZIOAppDebug:
  override val bootstrap =
    happyPath
  
  def run =
    effect6
  // Result: (PT5.031418578S,User saved)


val effect7 =
  effect6.when(userName != "Morty")

object App10 extends helpers.ZIOAppDebug:
  override val bootstrap =
    happyPath
  
  def run =
    effect7
  // Result: None


val program =
  defer:
    Console.printLine("Hello").run
    Console.printLine("world").run

object App11 extends helpers.ZIOAppDebug:
  val run =
    program
  // Hello
  // world


// NOTE: If you alter the sample below, you need to explicitly change the brittle error msg manipulation in Main
val x = 1 // This is just a dumb way to keep the code block from being empty, so it's properly hidden

object App12 extends helpers.ZIOAppDebug:
  def run =
    defer:
      program.repeatN(1).run
  // Hello
  // world
  // Hello
  // world


val surroundedProgram =
  defer:
    Console.printLine("**Before**").run
    program.repeatN(1).run
    Console.printLine("**After**").run

object App13 extends helpers.ZIOAppDebug:
  def run =
    surroundedProgram
  // **Before**
  // Hello
  // world
  // Hello
  // world
  // **After**
