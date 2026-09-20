package utility

import chisel3._
import chisel3.experimental.paramchoice._

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
