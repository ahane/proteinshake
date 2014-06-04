package eu.hanefeld.ba
import cc.factorie.variable.CategoricalDomain
/**
 * Created by alec on 5/30/14.
 */
object Types {
  type Spin = eu.hanefeld.ba.Spin
  type SpinValue = Char
  type SpinSequence = IndexedSeq[Spin]
  type SpinDomain = CategoricalDomain[SpinValue]
  type Distances = Map[(Int, Int), Double]
}
