package eu.hanefeld.ba

import cc.factorie.GibbsSampler
import cc.factorie.infer.Sampler
import cc.factorie.optimize.{AdaGrad, AdaGradRDA, Trainer}
import cc.factorie.la.DenseTensor2
import eu.hanefeld.ba.Types.SpinValue
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.JsonAST.{JObject, JField, JString, JDouble}

/**
 * Created by alec on 6/10/14.
 */
abstract class ConnectionPredictor(val msa: MSA, saveToFile: Boolean=true, fileext: String="pred1") {

  def strengths: Map[(Int, Int), Double]

  val positives: Set[(Int, Int)] = msa.connections

  def strengthsDescending: List[((Int, Int), Double)] = strengths.toList.sortBy(_._2).reverse

  def connectionsWithStrengthsOfAtLeast(threshold: Double): Set[(Int, Int)] = strengths.toList.filter(_._2 >= threshold).map(_._1).toSet

  def TPRate(upUntil: Int): Double = {
    val numPositives = positives.size
    val until = if(upUntil > numPositives) numPositives else upUntil
    val predicted = strengthsDescending.slice(0, until).map(_._1).toSet
    val truePositives = positives intersect predicted
    val tpRate = truePositives.size.toDouble / numPositives
    tpRate
  }
  def TPRate: Double = { TPRate(positives.size) }

  def saveResultsToFile(nameext: String): Unit = {
    import scalax.io._
    val out: Output = Resource.fromFile("pred/"+msa.name+"/"+ nameext + ".json" )
    out.write(getJson)

  }

  def getJson(): String = {
    val name = msa.name
    val numSites = msa.sequences(0).length
    val connectionsStrings = positives.toList.map(p => p._1.toString + ";" + p._2.toString)
    val predictionsMapStrings = (for((key, strength) <- strengths.toList) yield (key._1.toString +";"+key._2.toString, strength)).toMap
    val predictionsJson = (for((key, strength) <- predictionsMapStrings.toList) yield JObject(JField(key, JDouble(strength)) :: Nil)).toList
    val json = (
        ("name" -> name) ~
        ("postivives" -> connectionsStrings) ~
        ("predictions" -> predictionsJson))
    return pretty(render(json))
  }

}

class ContrastiveDivergenceConnectionPredictor(
  msa: MSA,
  potentialConnections: Set[(Int, Int)]=Set(),
  useLogFreqsAsLocalWeights: Boolean=true,
  numIterations: Int=3,
  learningRate: Double=1,
  l1: Double=0.01,
  l2: Double=0.000001,
  useParallelTrainer: Boolean=true,
  opt: String="AdaGradRDA") extends ConnectionPredictor(msa) {

  implicit val random = new scala.util.Random(0)

  val useAllConnections = (potentialConnections.size == 0)
  val model = if(useAllConnections) PottsModel(msa, useLogFreqsAsLocalWeights, excludeNeighbors=true) else PottsModel(msa,useLogFreqsAsLocalWeights, potentialConnections)

  val sequences = msa.sequences
  val sampler = new GibbsSampler(model).asInstanceOf[Sampler[Spin]]
  val CDExamples = sequences.map(new ContrastiveDivergenceExampleVector(_, model, sampler))
  val optimizer = new AdaGradRDA(rate=learningRate, l1 = l1, l2=l2, numExamples = msa.sequences.length)

  Trainer.onlineTrain(model.parameters, CDExamples,
    optimizer = optimizer, useParallelTrainer = useParallelTrainer, maxIterations = numIterations)

  val frobeniusNorms = ConnectionStrengths(model)
  def strengths = frobeniusNorms
}

class MututalInformationConnectionPredictor(msa: MSA, excludeNeighbors: Boolean) extends ConnectionPredictor(msa) {

  private val sequences = msa.sequences
  private val d = msa.domain
  private val MIs = computeMIs

  private def computeMIs = {
    val freqModel = PottsModel.frequenciesAsWeights(sequences, d, excludeNeighbors)

    def localFreqs(i: Int) = freqModel.localFamilies(i).weights.value
    def pairwiseFreqs(i: Int, j: Int) = freqModel.pairwiseFamilies(i, j).weights.value

    def singleMI(i: Int, j: Int): Double = {
      def f_i(k: SpinValue) = localFreqs(i)(d.index(k))
      def f_j(k: SpinValue) = localFreqs(j)(d.index(k))
      def f_ij(k: SpinValue, l: SpinValue) = pairwiseFreqs(i, j)(d.index(k), d.index(l))

      def summand(k: SpinValue, l: SpinValue): Double = {
        val summand = f_ij(k, l) * math.log( f_ij(k, l) / ( f_i(k) * f_j(l) ))
        summand
      }

      val MI = (for(k <- d.categories; l <- d.categories) yield summand(k, l)).sum
      MI
    }
    val edges = freqModel.pairwiseFamilies.keys
    val allMIs: Map[(Int, Int), Double] = (for(edge <- edges) yield (edge, singleMI(edge._1, edge._2))).toMap
    allMIs
}

  def strengths = { MIs }
    //we abuse the PottsModel infrastructure for convenient counting of pairwise and single-site frequencies

}

/**
 * This class is a rewrite of factories's ContrastiveDivergenceExample to support multiple variables in one example
 * might push upstream
 */
import cc.factorie.la.WeightsMapAccumulator
import cc.factorie.util.DoubleAccumulator
import cc.factorie.model.{DotFamily, Model, Parameters}
import cc.factorie.infer.Sampler
import cc.factorie.optimize.Example
class ContrastiveDivergenceExampleVector[C](val contexts: Iterable[C], model: Model with Parameters, val sampler: Sampler[C], val k: Int = 1) extends Example {
  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
    require(gradient != null, "The ContrastiveDivergenceExample needs a gradient accumulator")
    //val proposalDiff = new DiffList
    //repeat(k) { proposalDiff ++= sampler.process(context) }
    val proposalDiff = sampler.processAll(contexts, returnDiffs=true)
    model.factorsOfFamilyClass[DotFamily](proposalDiff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics, -1.0))
    proposalDiff.undo()
    model.factorsOfFamilyClass[DotFamily](proposalDiff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics))
  }
}


object ConnectionPredictor {
  //def apply(msa: MSA): ConnectionPredictor {


}
