package Chapter10_Appendix_RunningEffects

import zio.*
import zio.direct.*
import zio.test.*

object Test0 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("random is random"):
      defer:
        ZIO.debug("** logic **").run
        assertTrue:
          10 > 2
  // ** logic **
  // + random is random


object Test1 extends ZIOSpecDefault:
  // TODO This is blowing up.
  import zio.test.*
  
  def spec =
    test("console works"):
      defer:
        TestConsole
          .feedLines:
            "Zeb"
          .run
  
        logic.run
  
        val capturedOutput: String =
          TestConsole.output.run.mkString
        val expectedOutput =
          s"""|Enter your name
Hello Zeb
""".stripMargin
        assertTrue:
          capturedOutput == expectedOutput
  // - console works
  //   Exception in thread "zio-fiber-814143976" scala.NotImplementedError: an implementation is missing
  //   	at scala.Predef$.$qmark$qmark$qmark(Predef.scala:344)
  //   	at mdoctools.OurConsole.print(OurConsole.scala:14)
  //   	at zio.Console$.print$$anonfun$6(Console.scala:122)
  //   	at zio.ZIO$.consoleWith$$anonfun$1(ZIO.scala:3121)
  //   	at zio.FiberRef$unsafe$$anon$2.getWith$$anonfun$1(FiberRef.scala:474)
  //   	at repl.MdocSession.MdocApp.logic(<input>:72)
  //   	at zio.direct.ZioMonad.Success.$anon.flatMap(ZioMonad.scala:19)
  //   	at repl.MdocSession.MdocApp.logic(<input>:82)
  //   	at zio.direct.ZioMonad.Success.$anon.flatMap(ZioMonad.scala:19)
  //   	at repl.MdocSession.MdocApp.Chapter59Spec.spec(<input>:119)
