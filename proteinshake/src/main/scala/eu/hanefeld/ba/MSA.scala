package eu.hanefeld.ba
import eu.hanefeld.ba.Types._
import cc.factorie.variable.CategoricalDomain

/**
 * A Multiple Sequence Alignment is a collection of samples from protein domain
 * This is the highest abstraction for our data and holds
 *
 */
case class MSAJson(
            sequences: List[String],
            connections: List[String],
            name: String)

class MSA(
  val sequences: List[SpinSequence],
  val connections: Set[(Int, Int)],
  val domain: SpinDomain,
  val name: String) {

  //val weightedSequences =

  private def similarity(seq1: SpinSequence, seq2: SpinSequence): Double ={
    assert(seq1.length == seq2.length)
    val numSites = seq1.length
    val same = (for(i <- 0 until numSites if seq1(i) == seq2(i)) yield 1).sum
    same/numSites
  }
  //the threshold comes from [Weigt2011]
  private def numSimilarSequences(seq1: SpinSequence, threshold: Double=0.8): Int = {
    val similarities = for(seq2 <- sequences) yield similarity(seq1, seq2)
    val overThreshold = for(sim <- similarities) yield if(sim >= threshold) 1 else 0
    return overThreshold.sum
  }
}


object MSA {
  def apply(jsonMSA: MSAJson): MSA = {

    val domain = extractDomain(jsonMSA.connections)
    val spinSequences = stringSeqToSpinSeq(jsonMSA.sequences, domain)
    val connectionsSet = extractConnections(jsonMSA.connections)

    new MSA(spinSequences, connectionsSet, domain, jsonMSA.name)
  }

  //def apply(json: JObject): MSA
  def extractConnections(connectionsStrings: List[String]): Set[(Int, Int)] = {
    val connections = for(string <- connectionsStrings) yield (string.split(";")(0).toInt, string.split(";")(1).toInt)
    connections.toSet
  }

  def stringSeqToSpinSeq(stringSequences: List[String], domain: SpinDomain): List[SpinSequence] = {
    val s = stringSequences.map(stringSequence => Spin.makeSequence(stringSequence, domain))
    s
  }

  def extractDomain(stringSequences: List[String]): SpinDomain = {
    val domain = new CategoricalDomain[SpinValue]
    for(stringSequence <- stringSequences; spinChar <- stringSequence) domain += spinChar
    domain
  }

}