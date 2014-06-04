package eu.hanefeld.ba
import eu.hanefeld.ba.Types._
import cc.factorie.variable.CategoricalDomain

/**
 * A Multiple Sequence Alignment is a collection of samples from protein domain
 * This is the highest abstraction for our data and holds
 *
 */
class MSA(
  //a collection of SpinSequences
  val sequences: IndexedSeq[SpinSequence],

  //it mainly stores the true distances
  val trueDistances: ConnectionStrengths,
  //and a domain
  val domain: SpinDomain)

object MSA {
  def apply(sequences: IndexedSeq[String], trueDistances: ConnectionStrengths, domain: SpinDomain): MSA = {
    assert(sequences.forall(_.length == sequences(0).length), "Samples must all be of same length.")
    val spinSequences = sequences.map(stringSequence => Spin.makeSequence(stringSequence, domain))
    new MSA(spinSequences, trueDistances, domain)
  }

  def apply(sequences: IndexedSeq[String], trueDistances: ConnectionStrengths): MSA = {
    assert(sequences.forall(_.length == sequences(0).length), "Samples must all be of same length.")
    val domain: SpinDomain = new CategoricalDomain[SpinValue]
    for(stringSequence <- sequences; spinChar <- stringSequence) domain += spinChar
    //val spinSequences = sequences.map(stringSequence => Spin.makeSequence(stringSequence, domain))
    MSA(sequences, trueDistances, domain)
  }
}