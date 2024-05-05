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
  Config.Optional[Scenario](Config.fail("no default scenario"))

class StaticConfigProvider(scenario: Scenario) extends ConfigProvider:
  override def load[A](config: Config[A])(implicit trace: Trace): IO[Config.Error, A] =
    ZIO.succeed(Some(scenario).asInstanceOf[A])

val happyPath =
  Runtime.setConfigProvider(StaticConfigProvider(Scenario.HappyPath))

val neverWorks =
  Runtime.setConfigProvider(StaticConfigProvider(Scenario.NeverWorks))

val slow =
  Runtime.setConfigProvider(StaticConfigProvider(Scenario.Slow))

val doesNotWorkInitially =
  val scenario =
  Unsafe.unsafe {
    implicit unsafe =>
      Scenario.WorksOnTry(
        3,
        Runtime
          .default
          .unsafe
          .run(Ref.make(1))
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
   
      case Scenario.Slow =>
        ZIO.sleep(1.minute)
          .onInterrupt:
            ZIO.debug("Log: Interrupting slow request")
          .run
        succeed.run
    
      case Scenario.WorksOnTry(attempts, ref) =>
        val numCalls =
          ref.getAndUpdate(_ + 1).run
        if numCalls == attempts then
          succeed.run
        else
          fail.run
end saveUser

def sendToManualQueue(username: String) =
  ZIO
    .attempt(s"Please manually provision $username")

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

val effect1 =
  effect0.retryN(2)

val effect2 =
  effect1.orElseFail:
    "ERROR: User could not be saved"

val effect3 =
  effect2
    .timeoutFail("*** Save timed out ***"):
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
  // Result: User saved


object Example03_Superpowers_1 extends ZIOAppDefault:
  override val bootstrap =
    happyPath
  
  def run =
    effect0
  // Result: User saved


object Example03_Superpowers_2 extends ZIOAppDefault:
  override val bootstrap =
    neverWorks
  
  def run =
    effect0
  // Log: **Database crashed!!**
  // Result: **Database crashed!!**


object Example03_Superpowers_3 extends ZIOAppDefault:
  override val bootstrap =
    doesNotWorkInitially
  
  def run =
    effect1
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Result: User saved


object Example03_Superpowers_4 extends ZIOAppDefault:
  override val bootstrap =
    neverWorks
  
  def run =
    effect1
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Result: **Database crashed!!**


object Example03_Superpowers_5 extends ZIOAppDefault:
  override val bootstrap =
    neverWorks
  
  def run =
    effect2
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Result: ERROR: User could not be saved


object Example03_Superpowers_6 extends ZIOAppDefault:
  override val bootstrap =
    slow
  
  def run =
    effect3
  // Log: Interrupting slow request
  // Result: *** Save timed out ***


object Example03_Superpowers_7 extends ZIOAppDefault:
  override val bootstrap =
    neverWorks
  
  def run =
    effect4
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Log: **Database crashed!!**
  // Result: Please manually provision Morty


object Example03_Superpowers_8 extends ZIOAppDefault:
  override val bootstrap =
    happyPath
  
  def run =
    effect5
  // Log: Signup initiated for Morty
  // Result: User saved


object Example03_Superpowers_9 extends ZIOAppDefault:
  override val bootstrap =
    happyPath
  
  def run =
    effect6
  // Log: Signup initiated for Morty
  // Result: (PT5.053634102S,User saved)


object Example03_Superpowers_10 extends ZIOAppDefault:
  override val bootstrap =
    happyPath
  
  def run =
    effect7
  // Result: None
