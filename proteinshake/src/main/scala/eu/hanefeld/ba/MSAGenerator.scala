package eu.hanefeld.ba

/*import cc.factorie.variable.CategoricalDomain
import eu.hanefeld.ba.{PottsModel, WeightGenerator, Spin}
import cc.factorie.infer.GibbsSampler
import scala.collection.mutable.ArrayBuffer*/

/**
 * Created by alec on 3/23/14.
 */
class MSAGenerator {
/*val numSites: Int,
val domain: CategoricalDomain[Char],
val edgeProbability: Double,
val numSamples: Double = 5000,
val burnIn: Int = 1000,
val thinning: Int = 20)
(implicit val random: scala.util.Random) {

  val weightGenerator = WeightGenerator(domain, numSites, edgeProbability, false)
  val localWeights = weightGenerator.localMasses
  val pairwiseWeights = weightGenerator.pairwiseMasses

  val model = PottsModel(localWeights, pairwiseWeights, domain)

  val trueDistances = Distances(model)

  def samples: Seq[Spin] = {
    val sampler = new GibbsSampler(model)
    val samples = new ArrayBuffer[Spin]
    sampler.processAll(variables, burnIn)
    for(i <- 0 until N) {
      sampler.processAll(variables, thinning)
      strings += variables.map(_.value).mkString
    }
    strings
  }*/

}
