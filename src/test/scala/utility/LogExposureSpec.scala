package utility

import chisel3._
import chisel3.experimental.cacheable.{CacheableKey, CacheableModule}
import circt.stage.ChiselStage
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

object LogExposureSource {
  implicit object Key extends CacheableKey[LogExposureSource]
}

class LogExposureSource(implicit p: Parameters) extends Module with CacheableModule {
  val io = IO(new Bundle {})

  protected def buildModule(): Unit = {
    val value = RegInit(0.U(8.W))
    value := value + 1.U

    val debug = value === 1.U
    XSDebug(debug, p"debugValue=$value debugHex=${Hexadecimal(value)}\n")
    XSDebug(debug, p"debugMirror=$value\n")
    XSWarn(value === 2.U, "warningValue\n")
    XSError(value === 7.U, p"errorValue=$value\n")
  }
}

class LogExposureTop(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {})

  val source0 = CacheableModule(new LogExposureSource)
  val source1 = CacheableModule(new LogExposureSource)

  XSLog.collect(0.U, false.B, false.B, false.B)
}

class LogExposureSpec extends AnyFlatSpec with Matchers {
  private implicit val p: Parameters = Parameters.empty.alterPartial {
    case PerfCounterOptionsKey =>
      PerfCounterOptions(
        enablePerfPrint = false,
        enablePerfDB = false,
        perfLevel = XSPerfLevel.VERBOSE,
        perfDBHartID = 0
      )
    case LogUtilsOptionsKey =>
      LogUtilsOptions(
        enableDebug = true,
        enablePerf = false,
        fpgaPlatform = false,
        enableXMR = false
      )
  }

  private def sourceConnectionCount(chirrtl: String, source: String): Int = {
    val connection = raw"(?m)^\s*connect logEndpoint\.x_sink(?:_\d+)?\..*, $source\.x_event(?:_\d+)?\..*$$".r
    connection.findAllMatchIn(chirrtl).size
  }

  behavior of "LogUtils ExposureUtils routing"

  it should "materialize every log for every cached instance" in {
    val chirrtl = ChiselStage.emitCHIRRTL(new LogExposureTop)

    raw"(?m)^\s*module LogExposureSource\s*:".r.findAllMatchIn(chirrtl).size shouldBe 1
    raw"(?m)^\s*inst source\d+ of LogExposureSource\b".r.findAllMatchIn(chirrtl).size shouldBe 2
    raw"(?m)^\s*input x_sink(?:_\d+)?\s*:".r.findAllMatchIn(chirrtl).size shouldBe 8
    sourceConnectionCount(chirrtl, "source0") shouldBe 8
    sourceConnectionCount(chirrtl, "source1") shouldBe 8
    "debugValue=%d debugHex=%x".r.findAllMatchIn(chirrtl).size shouldBe 2
    "debugMirror=%d".r.findAllMatchIn(chirrtl).size shouldBe 2
    raw"(?m)^\s*printf\(.*\[DEBUG\].*debugValue=%d debugHex=%x.*debugMirror=%d".r
      .findAllMatchIn(chirrtl)
      .size shouldBe 2
    "warningValue".r.findAllMatchIn(chirrtl).size shouldBe 2
    "errorValue=%d".r.findAllMatchIn(chirrtl).size shouldBe 2
  }
}
