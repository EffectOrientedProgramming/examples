package Chapter03_Superpowers_kyo

import kyo.*

// start hidden
enum Scenario:
  case HappyPath
  case NeverWorks
  case Slow
  case WorksOnTry(attempts: Int)
  def apply(thing: String < (Env[Scenario] & IO & Abort[String] & Async & Var[Int])): Result[String, String] < Async =
    Env.run(this):
      Var.run(0):
        thing
    .handleAbort

def saveUser(username: String): String < (Env[Scenario] & IO & Abort[String] & Async & Var[Int]) =
  defer:
    await(Console.println(s"Attempting to save $username"))
    await(Env.get[Scenario]) match
      case Scenario.HappyPath =>
        await(IO("User saved"))
      case Scenario.NeverWorks =>
        await(Abort.fail("User not saved"))
      case Scenario.Slow =>
        await(Kyo.sleep(5.seconds))
        await(IO("User saved"))
      case Scenario.WorksOnTry(attempts) =>
        val count = await(Var.update[Int](_ + 1))
        if count < attempts then
          await(Abort.fail("User not saved"))
        else
          await(IO("User saved"))
// end hidden

val userName = "Morty"

val effect0 =
  saveUser:
    userName

object App0 extends KyoApp:
  run:
    Scenario.HappyPath:
      effect0

object App2 extends KyoApp:
  run:
    Scenario.WorksOnTry(attempts = 3):
      effect0

val retryPolicy = Retry.Policy.default.limit(2)
val effect1 = Retry[String](retryPolicy)(effect0)

object App3 extends KyoApp:
  run:
    Scenario.WorksOnTry(attempts = 3):
      effect1

object App4 extends KyoApp:
  run:
    Scenario.NeverWorks:
      effect1
