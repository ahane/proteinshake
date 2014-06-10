package eu.hanefeld.ba
import cc.factorie.variable.{CategoricalDomain, CategoricalVariable}
import eu.hanefeld.ba.Types._
/**
 * Represents one Spin or amino-acid value
 */
class Spin(c: Char, val i: Int, d: SpinDomain) extends CategoricalVariable[Char](c){

  //This is how FACTORIE assigns domains to variables.
  def domain = d

  //This is our own construct, to enable a spin to know what sample it is part of
  var sequence: SpinSequence = null
  def setSequence(sequence: SpinSequence): Unit = {this.sequence = sequence}

  override def toString = this.i.toString + ": " + this.value.toString
}

//Scala's companion objects allow for convenient Factory methods
object Spin {

  //The standard factory method, removes the need to use 'new' when creating instances.
  def apply(c: Char, i: Int, d: SpinDomain): Spin = new Spin(c, i, d)


  //Some factory methods vor creating spin vectors ie. protein samples.
  //This factory is used for to read in a single sample from the dataset.
  def makeSequence(s: String, d: SpinDomain): SpinSequence = {
    val spinSeq: SpinSequence = for((c, i) <- s.zipWithIndex) yield Spin(c, i, d)
    spinSeq.foreach(_.setSequence(spinSeq))
    spinSeq
  }

  def makeSequence(numSites: Int, d: SpinDomain, c: Char = 'a'): SpinSequence = {
    val spinSeq: SpinSequence = for(i <- 0 until numSites) yield Spin(c, i, d)
    spinSeq.foreach(_.setSequence(spinSeq))
    spinSeq
  }

}

