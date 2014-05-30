package eu.hanefeld.ba
import cc.factorie.variable.{CategoricalDomain, CategoricalVariable}
/**
 * Represents one Spin or amino-acid value
 */
class Spin(c: Char, val i: Int, d: CategoricalDomain[Char]) extends CategoricalVariable[Char](c){

  //This is how FACTORIE assigns domains to variables.
  def domain = d

  //This is our own construct, to enable a spin to know what sample it is part of
  var cont: IndexedSeq[Spin] = null
  def setContainer(container: IndexedSeq[Spin]): Unit = {this.cont = container}

  override def toString = this.i.toString + ": " + this.value.toString
}

//Scala's companion objects allow for convenient Factory methods
object Spin {

  //The standard factory method, removes the need to use 'new' when creating instances.
  def apply(c: Char, i: Int, d: CategoricalDomain[Char]): Spin = new Spin(c, i, d)


  //Some factory methods vor creating spin vectors ie. protein samples.


}

