package Chapter03_Superpowers_kyo

import kyo.*

import scala.reflect.ClassTag

// start hidden
enum Scenario:
  case HappyPath
  case NeverWorks
  case Slow
  case WorksOnTry(attempts: Int)
  // "injects" the scenario and a counter into the Effect
  def apply[A: Flat, E >: Nothing, S](effect: A < (Env[Scenario] & Abort[E] & Var[Int] & S))(using ClassTag[E], Tag[E]): Result[E, A] < S =
    Env.run(this):
      Var.run(0):
        Abort.run(effect)

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

val effect0 = saveUser("Morty")

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
