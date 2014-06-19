package eu.hanefeld.ba

import cc.factorie.variable.CategoricalDomain
import eu.hanefeld.ba.{PottsModel, WeightGenerator, Spin}
import cc.factorie.infer.GibbsSampler
import eu.hanefeld.ba.Types._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import scala.collection.mutable.ArrayBuffer
//import play.api.libs.json.Json
/**
 * Created by alec on 3/23/14.
 */
class MSAGenerator(
  val numSites: Int,
  val domain: SpinDomain,
  val name: String,
  val edgeProbability: Double,
  //val numSamples: Double = 5000,
  val burnInCount: Int=500,
  val thinningCount: Int=10)(implicit val random: scala.util.Random) {
  //implicit val random = new scala.util.Random(0)

  val weightGenerator = WeightGenerator(domain, numSites, edgeProbability)
  val localWeights = weightGenerator.generateLocalMasses
  val pairwiseWeights = weightGenerator.generatePairwiseMasses

  val model = PottsModel(localWeights, pairwiseWeights, domain)

  //val trueDistances = ConnectionStrengths(model, "bert")

  val sampler = new GibbsSampler(model)
  val spinSequence: SpinSequence = Spin.makeSequence(numSites, domain)
  //we draw burnInCount times and disregard the values
  println("--Starting burn in --")
  sampler.processAll(spinSequence, burnInCount)
  println("--burn in ended--")
  var sequences: IndexedSeq[String] = null
  var sequencesGenerated: Boolean = false
  def generateSequenceStrings(numSamples: Int = 2000): Unit = {
    sequencesGenerated = true
    sequences = for(i <- 0 until numSamples) yield drawString
  }
  private def drawString: String = {
    //we sample thinningCount times first
    sampler.processAll(spinSequence, thinningCount)
    return spinSequence.map(_.value).mkString
  }


  def getJSON: String = {
    if(!sequencesGenerated) { this.generateSequenceStrings() }
    val connections = pairwiseWeights.keys
    val connectionsStrings = connections.toList.map(c => c._1.toString + ";" + c._2.toString)
    val json = (
      ("sequences" -> sequences) ~
        ("name" -> name) ~
        ("connections" -> connectionsStrings) ~
        ("edge-prob" -> edgeProbability))
    return pretty(render(json))
  }
}

