package eu.hanefeld.ba

//import scala.collection.mutable.ArrayBuffer
//import cc.factorie.variable.{CategoricalDomain, Var}
//import cc.factorie.model.{Model, Parameters, Factor, DotFamilyWithStatistics1, DotFamilyWithStatistics2}
//import cc.factorie.la.{DenseTensor1, DenseTensor2}

/**
 * Created by alec on 3/22/14.
 */
class PottsModel {
/*(
                  val domain: CategoricalDomain[Char],
                  val localMasses: IndexedSeq[DenseTensor1],
                  val pairwiseMasses: Map[(Int, Int), DenseTensor2])
  extends Model with Parameters {

  /* We initialize our 'families' which contain an inner class to create factors on the fly.
   * There is a 1:1 relationship between the factors in our model and the families.
   * Wether or not two spins are connected is not defined in this class, it simply creates Families
   * for all the tensors it has received in its constructor argumetn pairwiseMasses. Those are created in a
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

    //a simple cast of varriables from Var to Spin
    val possibleNeighbours = variables collect {case s: Spin => s}

    for(s <- possibleNeighbours){
      val localFamily = localFamilies(s.i)
      factors += localFamily.Factor(s)

      val existingConnections = pairwiseFamilies.keys.filter((k: (Int, Int)) => k._1 == s.i || k._2 == s.i)
      for(k <- existingConnections) {
        val pairwiseFamily = pairwiseFamilies(k)
        factors += pairwiseFamily.Factor(s.cont(k._1), s.cont(k._2))
      }
    }
    factors
  }
}

object PottsModel {

  /* If we receive pairwises and local masses we create initialize the model. They might come from a synthetic model generator
   * or from the other factory methods below.
   */
  def apply(localMasses: IndexedSeq[DenseTensor1], pairwiseMasses: Map[(Int, Int), DenseTensor2], domain: CategoricalDomain[Char]): PottsModel = {
    new PottsModel(domain, localMasses, pairwiseMasses)
  }
  /* If just the number of variables and the domain is supplied, we create a completely connected model with weights initialized to zero.
   */
  def apply(numSites: Int, domain: CategoricalDomain[Char]): PottsModel = {
    val localMasses = for(i <- 0 until numSites) yield new DenseTensor1(domain.size)
    val pairwiseMasses = (for(i <- 0 until numSites; j <- 0 until numSites if i < j) yield ((i, j), new DenseTensor2(domain.size, domain.size))).toMap

    new PottsModel(domain, localMasses, pairwiseMasses)
  }

  /* This factory initializes a completely connected model, but initializes the local masses to be the the logarithms of
   * the local freuencies counted in a dataset. We add a pseudocount to deter limited sampling size effects.
   */
  def logFreqsAsLocalWeights(samples: Seq[IndexedSeq[Spin]], domain: CategoricalDomain[Char]): PottsModel = {
    val PSEUDOCOUNT = 2.
    assert(samples.forall(_.length == samples(0).length), "Samples must all be of same length.")
    val numSites = samples(0).length
    val numSamples = samples.toList.length
    val model = PottsModel(numSites, domain)

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


  /* We here abuse the tensor/factor infrastructure to collecte empirical frequencies from a dataset.
   * The "weights" of the this model are then used to estimate mutual information in the Experiment class.
   */
  def frequenciesAsWeights(samples: Seq[IndexedSeq[Spin]], domain: CategoricalDomain[Char]): PottsModel = {
    val PSEUDOCOUNT = 2.
    assert(samples.forall(_.length == samples(0).length), "Samples must all be of same length.")
    val numSites = samples(0).length
    val numSamples = samples.toList.length
    val model = PottsModel(numSites, domain)

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
*/
}