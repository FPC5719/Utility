/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2024 Institute of Computing Technology, Chinese Academy of Sciences
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package utility

import chisel3._
import chisel3.util._
import chisel3.util.experimental.ExposureUtils
import chisel3.experimental.BaseModule
import org.chipsalliance.cde.config.{Field, Parameters}
import XSLogLevel.XSLogLevel

import scala.collection.mutable.ListBuffer

case object LogUtilsOptionsKey extends Field[LogUtilsOptions]
case class LogUtilsOptions
(
  enableDebug: Boolean,
  enablePerf: Boolean,
  fpgaPlatform: Boolean,
  enableXMR: Boolean = true
)

object XSLogLevel extends Enumeration {
  type XSLogLevel = Value

  val ALL   = Value(0, "ALL  ")
  val DEBUG = Value("DEBUG")
  val INFO  = Value("INFO ")
  val PERF  = Value("PERF ")
  val WARN  = Value("WARN ")
  val ERROR = Value("ERROR")
  val OFF   = Value("OFF  ")
}

// Correspond to XSLog apply
case class LogPerfParam (
  debugLevel: XSLogLevel,
  prefix: Boolean,
  cond: Bool,
  fmt: String,
  data: Seq[Data],
  moduleTag: String
)

private[utility] final case class XSLogInfo(
  debugLevel: XSLogLevel,
  prefix: Boolean,
  fmt: String,
  moduleTag: String,
  module: BaseModule,
  condition: XSLogCondition,
  site: XSLogSite
) extends ExposureTag

private[utility] final class XSLogCondition

private[utility] final class XSLogSite

private[utility] final case class XSLogGroup(condition: XSLogCondition, occurrence: Int)

private[utility] final class XSLogEvent(private val dataTypes: Seq[Data]) extends Bundle {
  val cond = Bool()
  val data = MixedVec(dataTypes.map(_.cloneType))
}

private[utility] object XSLogEvent {
  def apply(cond: Bool, data: Seq[Data]): XSLogEvent = {
    val event = Wire(new XSLogEvent(data.map(x => chiselTypeOf(x))))
    event.cond := cond
    event.data.zip(data).foreach { case (sink, source) => sink := source }
    event
  }
}

class LogPerfIO extends Bundle {
  val timer = UInt(64.W)
  val logEnable = Bool()
  val clean = Bool()
  val dump = Bool()
}

private[utility] trait XSExposureHandle extends ExposureTag {
  def dataType: Data
}

private[utility] trait XSLogTap {
  def tapOrGet(handle: XSExposureHandle): Data = {
    val sink = Wire(handle.dataType.cloneType)
    ExposureUtils.expose(handle, isUpward = false, sink)
    sink
  }
}

private[utility] final class XSLogHandle(val dataType: Data) extends XSExposureHandle

private[utility] final case class XSLogRoute(info: XSLogInfo, handle: XSLogHandle, group: XSLogGroup)

private[utility] final case class XSLogEntry(param: LogPerfParam, group: XSLogGroup)

private[utility] object XSLogRouter extends XSLogTap {
  final case class Plan(routes: Seq[XSLogRoute], sources: Map[XSLogHandle, Data])

  private def groupSources(sources: Seq[(XSLogInfo, Data)]): Seq[(XSLogInfo, Data, XSLogGroup)] = {
    val seen = scala.collection.mutable.Map.empty[(XSLogCondition, XSLogSite), Int].withDefaultValue(0)

    sources.map { case (info, source) =>
      val key = info.condition -> info.site
      val occurrence = seen(key)
      seen(key) = occurrence + 1
      (info, source, XSLogGroup(info.condition, occurrence))
    }
  }

  private def materialize(info: XSLogInfo, source: Data, group: XSLogGroup): XSLogEntry = {
    val event = source.asInstanceOf[XSLogEvent]
    XSLogEntry(
      LogPerfParam(info.debugLevel, info.prefix, event.cond, info.fmt, event.data.toSeq, info.moduleTag),
      group
    )
  }

  def prepare(): Plan = {
    val routed = groupSources(ExposureUtils.collect[XSLogInfo]()).map { case (info, source, group) =>
      val handle = new XSLogHandle(chiselTypeOf(source))
      val routedInfo = info.copy(moduleTag = XSLog.routedModuleTag(info.module, source))
      (XSLogRoute(routedInfo, handle, group), handle -> source)
    }
    Plan(routed.map(_._1), routed.map(_._2).toMap)
  }

  def emit(routes: Seq[XSLogRoute]): Seq[XSLogEntry] = {
    routes.map(route => materialize(route.info, tapOrGet(route.handle), route.group))
  }

  def collectLocal(): Seq[XSLogEntry] = {
    groupSources(ExposureUtils.collect[XSLogInfo]()).map { case (info, source, group) =>
      materialize(info, source, group)
    }
  }

  def connect(plan: Plan): Unit = {
    val sinks = ExposureUtils.collect[XSLogHandle]().groupMap(_._1)(_._2)
    require(sinks.keySet == plan.sources.keySet, "Log exposure routes did not meet at their collection point")
    plan.sources.foreach { case (handle, source) =>
      val handleSinks = sinks(handle)
      require(handleSinks.size == 1, s"Log exposure route has ${handleSinks.size} sinks, expected one")
      handleSinks.head := source
    }
  }
}

object XSLog {
  private val callBacks = ListBuffer.empty[(LogPerfIO) => Unit]
  private val callBacksWithClock = ListBuffer.empty[(LogPerfIO, Reset, Clock) => Unit]
  private val logModules = ListBuffer.empty[BaseModule]
  private val routedModuleTags = scala.collection.mutable.Map.empty[String, () => String]
  private val routedModuleTagPattern = "__XSLOG_MODULE_[0-9]+__".r
  private var nextRoutedModuleTag = 0
  private val logConditions = new java.util.IdentityHashMap[Bool, XSLogCondition]

  private def conditionTag(cond: Bool): XSLogCondition = {
    val existing = logConditions.get(cond)
    if (existing != null) existing
    else {
      val created = new XSLogCondition
      logConditions.put(cond, created)
      created
    }
  }

  private[utility] def routedModuleTag(origin: BaseModule, source: Data): String = {
    val token = s"__XSLOG_MODULE_${nextRoutedModuleTag}__"
    nextRoutedModuleTag += 1
    routedModuleTags(token) = { () =>
      val originPath = origin.pathName
      val sourcePath = source.pathName
      val separator = sourcePath.lastIndexOf('.')
      val carrierPath = if (separator >= 0) sourcePath.substring(0, separator) else source.parentPathName
      if (originPath == carrierPath || originPath.startsWith(carrierPath + ".")) {
        originPath
      } else {
        val nestedPath = originPath.dropWhile(_ != '.')
        carrierPath + nestedPath
      }
    }
    token
  }

  private def unpackPrintable(pable: Printable): (String, Seq[Data]) = {
    val fmt = ListBuffer.empty[String]
    val data = ListBuffer.empty[Data]
    pable match {
      case Printables(p) => p.foreach { pb =>
        val (subfmt, subdata) = unpackPrintable(pb)
        fmt += subfmt
        data ++= subdata
      }
      case y: FirrtlFormat => {
        data += WireInit(y.bits)
        if      (y.isInstanceOf[Decimal])     fmt += "%d"
        else if (y.isInstanceOf[Hexadecimal]) fmt += "%x"
        else if (y.isInstanceOf[Binary])      fmt += "%b"
        else if (y.isInstanceOf[Character])   fmt += "%c"
      }
      case PString(str) => fmt += str
      case other => throw new IllegalArgumentException("XSLog not support unpack: " + other.toString)
    }
    (fmt.mkString(""), data.toSeq)
  }

  // curModOpt lets deferred perf logs retain the producer's module name when emitted at the endpoint.
  def apply(debugLevel: XSLogLevel, curModOpt: Option[BaseModule])
           (prefix: Boolean, cond: Bool, pable: Printable)(implicit p: Parameters): Unit = {
    val logOpts = p(LogUtilsOptionsKey)
    val enableDebug = logOpts.enableDebug && debugLevel != XSLogLevel.PERF
    val enablePerf = logOpts.enablePerf && debugLevel == XSLogLevel.PERF
    if (!logOpts.fpgaPlatform && (enableDebug || enablePerf || debugLevel == XSLogLevel.ERROR)) {
      require(chisel3.XSCompatibility.currentWhen.isEmpty,
        "XSLog to be collect not supported inside whenContext, use XSLog(cond, pable) instead")
      require(chisel3.XSCompatibility.currentModule.isDefined, "XSLog should be called inside a module")
      val curMod = curModOpt.getOrElse(chisel3.XSCompatibility.currentModule.get)
      if (!logModules.contains(curMod)) logModules += curMod
      val (fmt, data) = unpackPrintable(pable)
      val moduleTag = curMod.toString
      // Deferred and buffered apply()
      if (debugLevel == XSLogLevel.ERROR) {
        when(cond) {
          assert(false.B) //assert at current module for better error location
        }
      }
      ExposureUtils.expose(
        XSLogInfo(debugLevel, prefix, fmt, moduleTag, curMod, conditionTag(cond), new XSLogSite),
        isUpward = true,
        XSLogEvent(cond, data)
      )
    }
  }

  def apply(debugLevel: XSLogLevel)
           (prefix: Boolean, cond: Bool, pable: Printable)(implicit p: Parameters): Unit = {
    apply(debugLevel, None)(prefix, cond, pable)
  }

  def collect(ctrl: LogPerfIO)(implicit p: Parameters): Unit = {
    val logPlan = XSLogRouter.prepare()
    val perfPlan = XSPerfRouter.prepare()
    val logEndpoint = Module(new LogPerfEndpoint(logPlan.routes, perfPlan.routes))
    logEndpoint.io := ctrl
    XSLogRouter.connect(logPlan)
    XSPerfRouter.connect(perfPlan)
  }

  def collect(timer: UInt, logEnable: Bool, clean: Bool, dump: Bool)(implicit p: Parameters): Unit = {
    val ctrl = Wire(new LogPerfIO)
    ctrl.timer := timer
    ctrl.logEnable := logEnable
    ctrl.clean := clean
    ctrl.dump := dump
    collect(ctrl)
  }

  // Utilities that depend on LogPerfIO may defer hardware generation until collection.
  def registerCaller(caller: LogPerfIO => Unit): Unit = callBacks += caller
  def registerCallerWithClock(caller: (LogPerfIO, Reset, Clock) => Unit): Unit = callBacksWithClock += caller
  def invokeCaller(ctrl: LogPerfIO): Unit = callBacks.foreach(caller => caller(ctrl))
  def invokeCallerWithClock(ctrl: LogPerfIO, reset: Reset, clock: Clock) = callBacksWithClock.foreach(caller => caller(ctrl, reset, clock))

  // Should only be called during firrtl phase(ChiselStage)
  // PathName can not be accessed until circuit elaboration
  def replaceFIRStr(str: String): String = {
    val modulesReplaced = logModules.foldLeft(str) { case (acc, mod) =>
      acc.replace(mod.toString, mod.pathName)
    }
    routedModuleTagPattern.replaceAllIn(modulesReplaced, matched =>
      java.util.regex.Matcher.quoteReplacement(routedModuleTags(matched.matched)())
    )
  }
}

sealed abstract class LogHelper(val logLevel: XSLogLevel){

  def apply(cond: Bool, fmt: String, data: Bits*)(implicit p: Parameters): Unit =
    apply(cond, Printable.pack(fmt, data:_*))
  def apply(cond: Bool, pable: Printable)(implicit p: Parameters): Unit = apply(true, cond, pable)
  def apply(fmt: String, data: Bits*)(implicit p: Parameters): Unit =
    apply(Printable.pack(fmt, data:_*))
  def apply(pable: Printable)(implicit p: Parameters): Unit = apply(true.B, pable)
  def apply(prefix: Boolean, cond: Bool, fmt: String, data: Bits*)(implicit p: Parameters): Unit =
    apply(prefix, cond, Printable.pack(fmt, data:_*))
  def apply(prefix: Boolean, cond: Bool, pable: Printable)(implicit p: Parameters): Unit ={
    XSLog(logLevel)(prefix, cond, pable)
  }
}

object XSDebug extends LogHelper(XSLogLevel.DEBUG)

object XSInfo extends LogHelper(XSLogLevel.INFO)

object XSWarn extends LogHelper(XSLogLevel.WARN)

object XSError extends LogHelper(XSLogLevel.ERROR)

private class LogPerfEndpoint(logRoutes: Seq[XSLogRoute], perfRoutes: Seq[XSPerfRoute])(implicit p: Parameters)
    extends Module {
  val io = IO(Input(new LogPerfIO))
  def concatAndPrint(entries: Seq[XSLogEntry]): Unit = {
    entries.grouped(1000).foreach { entries =>
      val pable = entries.map { entry =>
        val info = entry.param
        val commonInfo = p"[${info.debugLevel}][time=${io.timer}] ${info.moduleTag}: "
        (if (info.prefix) commonInfo else p"") + Printable.pack(info.fmt, info.data: _*)
      }.reduce(_ + _)
      printf(pable)
    }
  }

  // Invoke legacy deferred collectors, then materialize the routed performance counters.
  XSLog.invokeCallerWithClock(io, reset, clock)
  XSLog.invokeCaller(io)
  val routedLogInfos = XSLogRouter.emit(logRoutes)
  XSPerfRouter.emit(perfRoutes, io)
  val logEntries = routedLogInfos ++ XSLogRouter.collectLocal()
  // Group printfs with same cond to reduce system tasks for better thread schedule
  logEntries.groupBy(_.group).values.foreach { entries =>
    val cond = entries.head.param.cond
    val errs = entries.filter(_.param.debugLevel == XSLogLevel.ERROR)
    if (!errs.isEmpty) {
      when (cond) {
        concatAndPrint(errs)
        // assert at local module for better error location
      }
    }
    val logs = entries.filterNot(_.param.debugLevel == XSLogLevel.ERROR)
    if (!logs.isEmpty) {
      when(io.logEnable && cond) {
        concatAndPrint(logs)
      }
    }
  }
}
