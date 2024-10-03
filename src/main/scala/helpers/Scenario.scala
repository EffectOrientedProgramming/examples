package helpers

import kyo.*

import scala.reflect.ClassTag

case class Counter(var current: Int)

enum Scenario:
  case HappyPath
  case NeverWorks
  case Slowly
  case WorksOnTry(attempts: Int, state: Counter = Counter(0))
  // "injects" the scenario and a counter into the Effect
  def apply[A: Flat, E >: Nothing, S](effect: A < (Env[Scenario] & Abort[E] & S))(using ClassTag[E], Tag[E]): Result[E, A] < S =
    Env.run(this):
      Abort.run(effect)

object Scenario:
  def Slow[A: Flat, S](effect: A < (Env[Scenario] & Abort[String | Timeout] & S)): Result[String, A] < S =
    Env.run(Scenario.Slowly):
      Abort.run:
        effect.catchSomeAbort[Timeout]()(_ => Abort.fail("Timed out"))

def saveUser(username: String): String < (Env[Scenario] & IO & Abort[String] & Async) =
  defer:
    await(Console.println(s"Attempting to save $username"))
    await(Env.get[Scenario]) match
      case Scenario.HappyPath =>
        await(IO("User saved"))
      case Scenario.NeverWorks =>
        await(Abort.fail("User not saved"))
      case Scenario.Slowly =>
        await(Kyo.sleep(5.seconds))
        await(IO("User saved"))
      case Scenario.WorksOnTry(attempts, counter) =>
        counter.current += 1
        if counter.current < attempts then
          await(Abort.fail("User not saved"))
        else
          await(IO("User saved"))
