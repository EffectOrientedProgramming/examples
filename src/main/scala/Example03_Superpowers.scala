import zio.*
import zio.direct.*

enum Scenario:
  case HappyPath
  case NeverWorks
  case NumberOfSlowCall(ref: Ref[Int])
  case WorksOnTry(attempts: Int, ref: Ref[Int])

// This configuration is used by effects to get the scenario that
// may have been passed in via `bootstrap`
// The configuration is optional and the default of `Config.fail`
// sets the Option to None.
val scenarioConfig: Config[Option[Scenario]] =
  Config.Optional[Scenario](Config.fail("no default scenario"))

class StaticConfigProvider(scenario: Scenario) extends ConfigProvider:
  override def load[A](config: Config[A])(implicit trace: Trace): IO[Config.Error, A] =
    ZIO.succeed(Some(scenario).asInstanceOf[A])

val happyPath =
  Runtime.setConfigProvider(StaticConfigProvider(Scenario.HappyPath))

val neverWorks =
  Runtime.setConfigProvider(StaticConfigProvider(Scenario.NeverWorks))

val doesNotWorkInitially =
  val scenario =
  Unsafe.unsafe {
    implicit unsafe =>
      Scenario.WorksOnTry(
        3,
        Runtime
          .default
          .unsafe
          .run(Ref.make(0))
          .getOrThrow()
      )
  }
  Runtime.setConfigProvider(StaticConfigProvider(scenario))

val firstIsSlow =
  val scenario =
    Unsafe.unsafe {
      implicit unsafe =>
        Scenario.NumberOfSlowCall(
          Runtime
            .default
            .unsafe
            .run(Ref.make(0))
            .getOrThrow()
        )
    }
  Runtime.setConfigProvider(StaticConfigProvider(scenario))

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
  defer:
    val maybeScenario = ZIO.config(scenarioConfig).run
    maybeScenario.getOrElse(Scenario.HappyPath) match
      case Scenario.HappyPath =>
        succeed.run

      case Scenario.NeverWorks =>
        fail.run
   
      case scenario: Scenario.NumberOfSlowCall =>
        val numCalls =
          scenario.ref.getAndUpdate(_ + 1).run
        if numCalls == 0 then
          ZIO.never.run
        else
          Console.printLine("Log: Database Timeout").run
          succeed.run
    
      case Scenario.WorksOnTry(attempts, ref) =>
        val numCalls =
          ref.getAndUpdate(_ + 1).run
        if numCalls == attempts then
          succeed.run
        else
          fail.run
  .onInterrupt:
    ZIO.debug("Log: Interrupting slow request")
end saveUser

def sendToManualQueue(username: String) =
  ZIO
    .attempt("User sent to manual setup queue")

val logUserSignup =
  Console.printLine:
    s"Log: Signup initiated for $userName"
  .orDie

// TODO Decide how much to explain this in the
// prose,
// without revealing the implementation
extension [R, E, A](z: ZIO[R, E, A])
  def fireAndForget(
      background: ZIO[R, Nothing, Any]
  ) =
    z.zipParLeft(background.forkDaemon)

val userName =
  "Morty"

val effect0 =
  saveUser:
    userName

object MyApp extends ZIOAppDefault:
  def run =
    effect0

import Schedule.{recurs, spaced}
val effect1 =
  effect0.retry:
    recurs(3) && spaced(1.second)

val effect2 =
  effect1.orElseFail:
    "ERROR: User could not be saved"

val effect3 =
  effect2.timeoutFail("Save timed out"):
    5.seconds

val effect4 =
  effect3.orElse:
    sendToManualQueue:
      userName

val effect5 =
  effect4.fireAndForget:
    logUserSignup

val effect6 =
  effect5.timed

val effect7 =
  effect6.when(userName != "Morty")

object Example03_Superpowers_0 extends ZIOAppDefault:
  def run =
    effect0
  // Result: Success(User saved)


object Example03_Superpowers_1 extends ZIOAppDefault:
  override val bootstrap =
    happyPath
  
  def run =
    effect0
  // Result: Success(User saved)


object Example03_Superpowers_2 extends ZIOAppDefault:
  override val bootstrap =
    neverWorks
  
  def run =
    effect0
  // Log: **Database crashed!!**
  // Result: Failure(Fail(**Database crashed!!**,Stack trace for thread "zio-fiber-193":
  // 	at repl.MdocSession.MdocApp.saveUser.fail(<input>:74)
  // 	at repl.MdocSession.MdocApp.saveUser.fail(<input>:78)
  // 	at repl.MdocSession.MdocApp.saveUser(<input>:105)
  // 	at mdoctools.ToRun.runSync.e(MdocHelpers.scala:63)
  // 	at mdoctools.ToRun.runSync.e(MdocHelpers.scala:64)
  // 	at mdoctools.ToRun.runSync(MdocHelpers.scala:69)))


object Example03_Superpowers_3 extends ZIOAppDefault:
  override val bootstrap =
    doesNotWorkInitially
  
  def run =
    effect1
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Result: Success(User saved)


object Example03_Superpowers_4 extends ZIOAppDefault:
  override val bootstrap =
    neverWorks
  
  def run =
    effect1
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Result: Failure(Fail(**Database crashed!!**,Stack trace for thread "zio-fiber-1040":
  // 	at repl.MdocSession.MdocApp.saveUser.fail(<input>:74)
  // 	at repl.MdocSession.MdocApp.saveUser.fail(<input>:78)
  // 	at repl.MdocSession.MdocApp.saveUser(<input>:105)
  // 	at repl.MdocSession.MdocApp.effect1(<input>:193)
  // 	at mdoctools.ToRun.runSync.e(MdocHelpers.scala:63)
  // 	at mdoctools.ToRun.runSync.e(MdocHelpers.scala:64)
  // 	at mdoctools.ToRun.runSync(MdocHelpers.scala:69)))


object Example03_Superpowers_5 extends ZIOAppDefault:
  override val bootstrap =
    neverWorks
  
  def run =
    effect2
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Result: Failure(Fail(ERROR: User could not be saved,Stack trace for thread "zio-fiber-1300":
  // 	at repl.MdocSession.MdocApp.effect2(<input>:231)
  // 	at mdoctools.ToRun.runSync.e(MdocHelpers.scala:63)
  // 	at mdoctools.ToRun.runSync.e(MdocHelpers.scala:64)
  // 	at mdoctools.ToRun.runSync(MdocHelpers.scala:69)))


object Example03_Superpowers_6 extends ZIOAppDefault:
  override val bootstrap =
    firstIsSlow
  
  def run =
    effect3
  // Log: Interrupting slow request
  // Result: Failure(Fail(Save timed out,Stack trace for thread "zio-fiber-1581":
  // 	at repl.MdocSession.MdocApp.effect3(<input>:254)
  // 	at mdoctools.ToRun.runSync.e(MdocHelpers.scala:63)
  // 	at mdoctools.ToRun.runSync.e(MdocHelpers.scala:64)
  // 	at mdoctools.ToRun.runSync(MdocHelpers.scala:69)))


object Example03_Superpowers_7 extends ZIOAppDefault:
  override val bootstrap =
    neverWorks
  
  def run =
    effect4
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Result: Success(User sent to manual setup queue)


object Example03_Superpowers_8 extends ZIOAppDefault:
  override val bootstrap =
    happyPath
  
  def run =
    effect5
  // Log: Signup initiated for Morty
  // Result: Success(User sent to manual setup queue)


object Example03_Superpowers_9 extends ZIOAppDefault:
  override val bootstrap =
    happyPath
  
  def run =
    effect6
  // Result: Success((PT0.047501388S,User sent to manual setup queue))


object Example03_Superpowers_10 extends ZIOAppDefault:
  override val bootstrap =
    happyPath
  
  def run =
    effect7
  // Result: Success(None)
