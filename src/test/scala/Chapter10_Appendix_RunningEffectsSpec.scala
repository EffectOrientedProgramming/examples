package Chapter10_Appendix_RunningEffects

import zio.*
import zio.direct.*
import zio.test.*

object Test0 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("random is random"):
      defer:
        assertTrue:
          Random.nextIntBounded(10).run < 10
  
  // TODO: Justify defer syntax over for-comp for multi-statement assertions
  //      Change this to a Console app, where the logic & testing is more visceral
  // + random is random


object Test1 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("random is still random"):
      defer:
        assertTrue:
          Random.nextIntBetween(0, 10).run <=
            10 &&
            Random
              .nextIntBetween(10, 20)
              .run <= 20 &&
            Random
              .nextIntBetween(20, 30)
              .run <= 30
  // + random is still random


object Test2 extends ZIOSpecDefault:
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
  //   Exception in thread "zio-fiber-1431620585" scala.NotImplementedError: an implementation is missing
  //   	at scala.Predef$.$qmark$qmark$qmark(Predef.scala:344)
  //   	at mdoctools.OurConsole.print(OurConsole.scala:14)
  //   	at zio.Console$.print$$anonfun$6(Console.scala:122)
  //   	at zio.ZIO$.consoleWith$$anonfun$1(ZIO.scala:3121)
  //   	at zio.FiberRef$unsafe$$anon$2.getWith$$anonfun$1(FiberRef.scala:474)
  //   	at repl.MdocSession.MdocApp.logic(<input>:100)
  //   	at zio.direct.ZioMonad.Success.$anon.flatMap(ZioMonad.scala:19)
  //   	at repl.MdocSession.MdocApp.logic(<input>:110)
  //   	at zio.direct.ZioMonad.Success.$anon.flatMap(ZioMonad.scala:19)
  //   	at repl.MdocSession.MdocApp.Chapter57Spec.spec(<input>:146)
