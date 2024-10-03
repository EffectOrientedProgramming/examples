package Chapter03_Superpowers

import kyo.*
import helpers.*

val effect0: String < (Env[Scenario] & Abort[String] & IO & Async) =
  saveUser("Morty")

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

val effect2 =
  Async.timeout(1.second)(effect1)

object App5 extends KyoApp:
  run:
    Scenario.Slow:
      effect2
