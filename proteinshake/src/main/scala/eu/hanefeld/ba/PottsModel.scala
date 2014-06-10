package eu.hanefeld.ba

import scala.collection.mutable.ArrayBuffer
import cc.factorie.variable.{CategoricalDomain, Var}
import cc.factorie.model.{Model, Parameters, Factor, DotFamilyWithStatistics1, DotFamilyWithStatistics2}
import cc.factorie.la.{DenseTensor1, DenseTensor2}
import eu.hanefeld.ba.Types._
import eu.hanefeld.ba.MSA

/**
 * Created by alec on 3/22/14.
 */
class PottsModel(
      val domain: CategoricalDomain[Char],
      val localMasses: IndexedSeq[DenseTensor1],
      val pairwiseMasses: Map[(Int, Int), DenseTensor2])
  extends Model with Parameters {

  /* We initialize our 'families' which contain an inner class to create factors on the fly.
   * There is a 1:1 relationship between the factors in our model and the families.
   * Whether or not two spins are connected is not defined in this class, it simply creates Families
   * for all the tensors it has received in its constructor argument pairwiseMasses. Those are created in a
   * factory method in the companion object below.
   */
  val localFamilies: IndexedSeq[DotFamilyWithStatistics1[Spin]] =
    for(m <- localMasses) yield new DotFamilyWithStatistics1[Spin] { val weights = Weights(m) }

  val pairwiseFamilies: Map[(Int, Int), DotFamilyWithStatistics2[Spin, Spin]] =
    for((k, m) <- pairwiseMasses) yield (k -> new DotFamilyWithStatistics2[Spin, Spin] { val weights = Weights(m) })


  /* The essantial method that has to be implemented by a any model. Here is where the 'imperative freedom' of IDF happens.
   * We chose to include a reference of all possible neighbours in the Spin.cont member. We then iterate over them
   * and see if our model contains a family for that neighbour, create a factor if it does and add it to the results.
   */
  def factors(variables: Iterable[Var]): Iterable[Factor] = {
    val factors = new ArrayBuffer[Factor]

    //a simple cast of variables from Var to Spin
    val spins = variables collect {case s: Spin => s}

    for(s <- spins){

      factors += initLocalFactor(s)

      val connections = findConnections(s)
      for(connection <- connections) {
        factors += initPairwiseFactor(connection, s)
      }
    }
    factors
  }
  private def initLocalFactor(s: Spin): Factor = {
    val localFamily = localFamilies(s.i)
    return localFamily.Factor(s)
  }

  /**
   * Inititalizes a Factor by looking at a Spin's container object and picking the Spins
   * indicated by the connection tuple.
   *
   * @param connection A tuple representing the neighbours of Spin s
   * @param s The Spin of which we want the factor
   * @return A Factor representing the interaction strength of
   */
  private def initPairwiseFactor(connection: (Int, Int), s: Spin): Factor = {
    val pairwiseFamily = pairwiseFamilies(connection)
    //Either s_1 or s_2 will be the same as s.
    val s_1: Spin = s.sequence(connection._1)
    val s_2: Spin = s.sequence(connection._2)
    return pairwiseFamily.Factor(s_1, s_2)
  }

  /**
   * Returns a iterable of (Int, Int) tuples, that represent all connections of a given Spin s
   * @param s
   */
  private def findConnections(s: Spin): Iterable[(Int, Int)] = {
    return pairwiseFamilies.keys.filter((k: (Int, Int)) => k._1 == s.i || k._2 == s.i)
  }
}

object PottsModel {

  /**
   * If we receive pairwise and local masses we create initialize the model. They might come from a synthetic model generator
   * or from the other factory methods below.
   */
  def apply(localMasses: IndexedSeq[DenseTensor1], pairwiseMasses: Map[(Int, Int), DenseTensor2], domain: SpinDomain): PottsModel = {
    new PottsModel(domain, localMasses, pairwiseMasses)
  }
  /**
   * If just the number of variables and the domain is supplied, we create a completely connected model with weights initialized to zero.
   */
  def apply(numSites: Int, domain: CategoricalDomain[Char], excludeNeighbors: Boolean = true, exclusionNeighborhood: Int = 4): PottsModel = {
    val localMasses = for(i <- 0 until numSites) yield new DenseTensor1(domain.size)
    val pairwiseMasses = generatePairs(numSites, exclusionNeighborhood).map((_, new DenseTensor2(domain.size, domain.size))).toMap
    new PottsModel(domain, localMasses, pairwiseMasses)
  }

  private def generatePairs(numSites: Int, exclusionNeighborhood: Int): Seq[(Int, Int)] = {


    def includePair(i: Int, j: Int, neighborhood: Int): Boolean = {
      orderingIsValid(i, j) && outsideOfNeighborhood(i, j, neighborhood)
    }
    def orderingIsValid(i: Int, j: Int): Boolean = { i < j }

    def outsideOfNeighborhood(i: Int, j: Int, neighborhood: Int): Boolean = {
      val distance = j - i
      val isOutside = distance > neighborhood
      if(isOutside) { true }
      else { false }

    }
    val pairs = for(i <- 0 until numSites; j <- 0 until numSites if includePair(i, j, exclusionNeighborhood)) yield (i, j)

    pairs
  }

  /* This factory initializes a completely connected model, but initializes the local masses to be the the logarithms of
   * the local freuencies counted in a dataset. We add a pseudocount to deter limited sampling size effects.\
   * DEPRECATED?
   */
  def logFreqsAsLocalWeights(msa: MSA, excludeNeighbors: Boolean = true, exclusionNeighborhood: Int = 4): PottsModel = {
    logFreqsAsLocalWeights(msa.sequences, msa.domain, excludeNeighbors, exclusionNeighborhood)
  }

  def logFreqsAsLocalWeights(samples: Seq[SpinSequence], domain: SpinDomain, excludeNeighbors: Boolean , exclusionNeighborhood: Int): PottsModel = {
    val PSEUDOCOUNT = 2.
    assert(samples.forall(_.length == samples(0).length), "Samples must all be of same length.")
    val numSites = samples(0).length
    val numSamples = samples.toList.length
    val model = PottsModel(numSites, domain, excludeNeighbors, exclusionNeighborhood)

    //
    for ((family, i) <- model.localFamilies.zipWithIndex) {
      val weightTensor = model.parameters(family.weights)

      val pseudoCountTensor = weightTensor.copy
      for(i <- 0 until pseudoCountTensor.asArray.length) pseudoCountTensor.update(i, PSEUDOCOUNT)
      weightTensor += pseudoCountTensor

      for(s <- samples) {
        val factor = family.Factor(s(i))
        weightTensor += factor.currentStatistics
      }
      weightTensor *= 1./(numSamples + (domain.size * PSEUDOCOUNT))

      for(i <- 0 until weightTensor.length) weightTensor.update(i, math.log(weightTensor(i)))
    }
    model
  }


  /* We here abuse the tensor/factor infrastructure to collect empirical frequencies from a dataset.
   * The "weights" of the this model are then used to estimate mutual information in the Experiment class.
   */
  def frequenciesAsWeights(samples: Seq[SpinSequence], domain: SpinDomain, excludeNeighbors: Boolean = true, exclusionNeighborhood: Int = 4): PottsModel = {
    val PSEUDOCOUNT = 2.
    assert(samples.forall(_.length == samples(0).length), "Samples must all be of same length.")
    val numSites = samples(0).length
    val numSamples = samples.toList.length
    val model = PottsModel(numSites, domain,  excludeNeighbors, exclusionNeighborhood)

    for ((key, family) <- model.pairwiseFamilies) {
      val weightTensor = model.parameters(family.weights)

      //we add a pseudocount as in [Weight08]
      val pseudoCountTensor = weightTensor.copy
      for(i <- 0 until pseudoCountTensor.asArray.length) pseudoCountTensor.update(i, (PSEUDOCOUNT/domain.size))
      weightTensor += pseudoCountTensor

      for(s <- samples) {
        val factor = family.Factor(s(key._1), s(key._2))
        weightTensor += factor.currentStatistics
      }
      //we correct our denominator by domain.size to account for the pseudocount
      weightTensor *= 1./(numSamples + (domain.size * PSEUDOCOUNT))
      for(i <- 0 until weightTensor.length) weightTensor.update(i, weightTensor(i))
    }

    for ((family, i) <- model.localFamilies.zipWithIndex) {
      val weightTensor = model.parameters(family.weights)

      val pseudoCountTensor = weightTensor.copy
      for(i <- 0 until pseudoCountTensor.asArray.length) pseudoCountTensor.update(i, PSEUDOCOUNT)
      weightTensor += pseudoCountTensor

      for(s <- samples) {
        val factor = family.Factor(s(i))
        weightTensor += factor.currentStatistics
      }
      weightTensor *= 1./(numSamples + (domain.size * PSEUDOCOUNT))
      for(i <- 0 until weightTensor.length) weightTensor.update(i, weightTensor(i))
    }
    model
  }

}