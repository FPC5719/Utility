package utility

import chisel3._
import chisel3.experimental.paramchoice._
import org.chipsalliance.cde.config.Field

/** The `Parameters` key carrying the single `HartIdDomain` instance of an elaboration.
  *
  * It lives in `utility` (and not in `xiangshan`) so that `XSCache` and `XiangShan` resolve the
  * *same* instance: a `ChoiceDomain` has to be created exactly once per elaboration, and every
  * `Property[domain.Case]` must be typed against that one instance. Only the top level provisions
  * it; everything below reads it through `HasCoupledL2Parameters`/`HasXSParameter`.
  */
case object XSHartIdDomainKey extends Field[HartIdDomain]

/** The set of `Case`s (one per hart) used to select the per-hart tables and records.
  *
  * A `ChoiceDomain` has to be created exactly once per elaboration: every instance registers
  * itself in `Builder.choiceDomains` and is emitted as its own `choice_domain` in the FIRRTL.
  * Build it once where the number of harts is known (inside elaboration) and pass the instance
  * to the modules through parameters; do not re-create it on every parameter lookup.
  */
class HartIdDomain(val numHarts: Int) extends ChoiceDomain {
  require(numHarts > 0, s"number of harts must be positive, but got $numHarts")
  val harts: IndexedSeq[Case] = (0 until numHarts).map { i => Case(s"Hart_$i") }
}
