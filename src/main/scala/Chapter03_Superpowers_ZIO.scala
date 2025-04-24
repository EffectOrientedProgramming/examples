package Chapter03_Superpowers_ZIO

import zio.*
import zio.direct.*
import helpers_zio.*
import helpers_zio.Scenario.*

val effect0 = saveUser("Morty")

object App0 extends ZIOAppDebug:
  def run =
    Successful.simulate:
      effect0


object App1 extends ZIOAppDebug:
  def run =
    WorksOnThirdTry.simulate:
      effect0


val effect1 = effect0.retryN(2)
//val effect1 = effect0.orElseSucceed("asdf").retryN(2)

object App2 extends ZIOAppDebug:
  def run =
    WorksOnThirdTry.simulate:
      effect1


object App3 extends ZIOAppDebug:
  def run =
    NeverWorks.simulate:
      effect1


val effect2 =
  effect1.orElseFail:
    "FAILURE: User not saved"

object App4 extends ZIOAppDebug:
  def run =
    NeverWorks.simulate:
      effect2


val effect3 =
  effect2
    .timeoutFail("** Save timed out **"):
      5.seconds

object App5 extends ZIOAppDebug:
  def run =
    Slow.simulate:
      effect3


val effect4 =
  effect3.orElse:
    sendToManualQueue("Morty")

object App6 extends ZIOAppDebug:
  def run =
    NeverWorks.simulate:
      effect4


val effect5 =
  effect4.withFinalizer:
    username => logUserSignup(username)

object App7 extends ZIOAppDebug:
  def run =
    Successful.simulate:
      effect5


val effect6 = effect5.timed

object App8 extends ZIOAppDebug:
  def run =
    Successful.simulate:
      effect6


object App10 extends ZIOAppDebug:
  def run =
    Successful.simulate:
      ZIO.debug("Before save")
      effect1


val effect8 =
  defer:
    ZIO.debug("Before save").run
    effect1.run

object App11 extends ZIOAppDebug:
  def run =
    Successful.simulate:
      effect8


object App12 extends ZIOAppDebug:
  def run =
    Successful.simulate:
      defer:
        ZIO.debug("**Before**").run
        effect8.debug.repeatN(1).run
        ZIO.debug("**After**").run
