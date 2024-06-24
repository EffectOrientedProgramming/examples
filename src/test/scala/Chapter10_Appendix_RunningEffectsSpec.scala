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
  // + random is random
  // Result: Summary(1,0,0,,PT0.298276S)


object Test1 extends ZIOSpecDefault:
  import zio.test.*
  
  def spec =
    test("random is still random"):
      defer:
        assertTrue:
          Random.nextIntBetween(0, 10).run <= 10 &&
          Random.nextIntBetween(10, 20).run <=
            20 &&
            Random.nextIntBetween(20, 30).run <= 30
  // + random is still random
  // Result: Summary(1,0,0,,PT0.142352S)


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
  //   Exception in thread "zio-fiber-1361130204" scala.NotImplementedError: an implementation is missing
  //   	at scala.Predef$.$qmark$qmark$qmark(Predef.scala:344)
  //   	at mdoctools.OurConsole.print(OurConsole.scala:14)
  //   	at zio.Console$.print$$anonfun$6(Console.scala:122)
  //   	at zio.ZIO$.consoleWith$$anonfun$1(ZIO.scala:3068)
  //   	at zio.FiberRef$unsafe$$anon$2.getWith$$anonfun$1(FiberRef.scala:474)
  //   	at repl.MdocSession.MdocApp.logic(<input>:91)
  //   	at zio.direct.ZioMonad.Success.$anon.flatMap(ZioMonad.scala:19)
  //   	at repl.MdocSession.MdocApp.logic(<input>:101)
  //   	at zio.direct.ZioMonad.Success.$anon.flatMap(ZioMonad.scala:19)
  //   	at repl.MdocSession.MdocApp.Chapter63Spec.spec(<input>:137)
  // Result: 
  // - console works
  //   Exception i
