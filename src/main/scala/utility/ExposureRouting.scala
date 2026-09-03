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
import chisel3.util.experimental.ExposureUtils

import scala.reflect.ClassTag

private abstract class XSExposureHandle(val dataType: Data) extends ExposureTag

private final case class XSExposurePlan[H <: XSExposureHandle, R](
  routes: Seq[R],
  sources: Map[H, Data]
)

private object XSExposureBridge {
  def sinkFor(handle: XSExposureHandle): Data = {
    val sink = Wire(handle.dataType.cloneType)
    ExposureUtils.expose(handle, isUpward = false, sink)
    sink
  }

  def connect[H <: XSExposureHandle: ClassTag, R](plan: XSExposurePlan[H, R], owner: String): Unit = {
    val sinks = ExposureUtils.collect[H]().groupMap(_._1)(_._2)
    require(sinks.keySet == plan.sources.keySet, s"$owner exposure routes did not meet at their collection point")
    plan.sources.foreach { case (handle, source) =>
      val handleSinks = sinks(handle)
      require(handleSinks.size == 1, s"$owner exposure route has ${handleSinks.size} sinks, expected one")
      handleSinks.head := source
    }
  }
}
