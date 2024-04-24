import zio.*
import zio.direct.*
import zio.test.*

object Example10_Appendix_RunningEffects_0 extends ZIOSpecDefault:
  def spec =
    test("random is random"):
      defer:
        assertTrue:
          Random.nextIntBounded(10).run < 10
  // Result: Test PASSED


object Example10_Appendix_RunningEffects_1 extends ZIOSpecDefault:
  def spec =
    test("random is still random"):
      defer:
        assertTrue:
          Random.nextIntBetween(0, 10).run <= 10 &&
          Random.nextIntBetween(10, 20).run <= 20 &&
          Random.nextIntBetween(20, 30).run <= 30
  // Result: Test PASSED


object Example10_Appendix_RunningEffects_2 extends ZIOSpecDefault:
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
  // Result: Test PASSED
