package utility

import chisel3._
import chisel3.experimental.cacheable.{CacheableKey, CacheableModule}
import circt.stage.ChiselStage
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

object PerfCounterExposureSource {
  implicit object Key extends CacheableKey[PerfCounterExposureSource]
}

class PerfCounterExposureSource(implicit p: Parameters) extends Module with CacheableModule {
  val io = IO(new Bundle {})

  protected def buildModule(): Unit = {
    val value = RegInit(0.U(8.W))
    value := value + 1.U

    XSPerfAccumulate("exposureAcc", value)
    XSPerfReference("exposureRef", value)
    XSPerfHistogram("exposureHist", value, true.B, start = 0, stop = 4)
    XSPerfMax("exposureMax", value, true.B)
  }
}

class PerfCounterExposureTop(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {})

  val source0 = CacheableModule(new PerfCounterExposureSource)
  val source1 = CacheableModule(new PerfCounterExposureSource)

  XSLog.collect(0.U, false.B, false.B, false.B)
}

class PerfCounterExposureSpec extends AnyFlatSpec with Matchers {
  private implicit val p: Parameters = Parameters.empty.alterPartial {
    case PerfCounterOptionsKey =>
      PerfCounterOptions(
        enablePerfPrint = true,
        enablePerfDB = false,
        perfLevel = XSPerfLevel.VERBOSE,
        perfDBHartID = 0
      )
    case LogUtilsOptionsKey =>
      LogUtilsOptions(
        enableDebug = false,
        enablePerf = true,
        fpgaPlatform = false,
        enableXMR = false
      )
  }

  private def declarationCount(chirrtl: String, kind: String, name: String): Int = {
    val declaration = raw"(?m)^\s*$kind ${name}(?:_\d+)?\s*:".r
    declaration.findAllMatchIn(chirrtl).size
  }

  private def sourceConnectionCount(chirrtl: String, source: String): Int = {
    val connection = raw"(?m)^\s*connect logEndpoint\.x_data(?:_\d+)?(?:\.\w+)?, $source\.x_\w+(?:\.\w+)?\b".r
    connection.findAllMatchIn(chirrtl).size
  }

  behavior of "PerfCounter ExposureUtils routing"

  it should "materialize a separate endpoint route for every cached instance" in {
    val chirrtl = ChiselStage.emitCHIRRTL(new PerfCounterExposureTop)

    declarationCount(chirrtl, "regreset", "exposureAccCounter") shouldBe 2
    declarationCount(chirrtl, "wire", "exposureRefOut") shouldBe 2
    declarationCount(chirrtl, "regreset", "exposureHistSum") shouldBe 2
    declarationCount(chirrtl, "regreset", "exposureHist_0_1") shouldBe 2
    declarationCount(chirrtl, "regreset", "max") shouldBe 2
    raw"(?m)^\s*input x_data(?:_\d+)?\s*:".r.findAllMatchIn(chirrtl).size shouldBe 8
    sourceConnectionCount(chirrtl, "source0") shouldBe 6
    sourceConnectionCount(chirrtl, "source1") shouldBe 6
    raw"(?m)^\s*inst source\d+ of PerfCounterExposureSource\b".r
      .findAllMatchIn(chirrtl)
      .size shouldBe 2
  }
}
