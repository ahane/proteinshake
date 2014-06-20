package eu.hanefeld.ba

import cc.factorie.GibbsSampler
import cc.factorie.infer.Sampler
import cc.factorie.optimize._
import cc.factorie.la.DenseTensor2
import cc.factorie._

import eu.hanefeld.ba.Types.SpinValue
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.JsonAST.{JObject, JField, JString, JDouble}
import cc.factorie.variable.DiffList
import org.json4s.JsonAST.JDouble

/**
 * Created by alec on 6/10/14.
 */
abstract class ConnectionPredictor(val msa: MSA, val excludeNeighbours: Boolean, val neighbourhood: Int) {

  def strengths: Map[(Int, Int), Double]

  val numSites = msa.sequences(0).length
  val includedConnections = PottsModel.generatePairs(numSites, excludeNeighbours, neighbourhood)
  val positives: Set[(Int, Int)] = includedConnections intersect msa.connections
  val numPositives = positives.size
  
  def strengthsDescending: List[((Int, Int), Double)] = strengths.toList.sortBy(_._2).reverse

  def connectionsWithStrengthsOfAtLeast(threshold: Double): Set[(Int, Int)] = strengths.toList.filter(_._2 >= threshold).map(_._1).toSet

  def TPRate(upUntil: Int): Double = {
    val until = if(upUntil > numPositives) numPositives else upUntil
    val predicted = strengthsDescending.slice(0, until).map(_._1).toSet
    val truePositives = positives intersect predicted
    val tpRate = truePositives.size.toDouble / numPositives
    tpRate
  }
  def TPRate: Double = { TPRate(numPositives) }

  def saveResultsToFile(nameext: String): Unit = {
    import scalax.io._
    val out: Output = Resource.fromFile("pred/"+msa.name+"/"+ nameext + ".json")
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
        ("predictions" -> predictionsJson) ~
        ("tp-rate" -> TPRate))
    return pretty(render(json))
  }


}

class ContrastiveDivergenceConnectionPredictor(
  msa: MSA,
  excludeNeighbours: Boolean,
  neighbourhood: Int,
  potentialConnections: Set[(Int, Int)]=Set(),
  numIterations: Int=3,
  learningRate: Double=1,
  l1: Double=0.01,
  l2: Double=0.000001,
  k: Int=1,
  useParallelTrainer: Boolean=true,
  useLogFreqs: Boolean=true) extends ConnectionPredictor(msa, excludeNeighbours, neighbourhood) {

  implicit val random = new scala.util.Random(0)

  val useAllConnections = (potentialConnections.size == 0)

  val model =
    if(useAllConnections) PottsModel(msa, useLogFreqs, excludeNeighbours, neighbourhood)
    else PottsModel(msa, useLogFreqs, potentialConnections)

  val sequences = msa.sequences
  val sampler = new GibbsSampler(model).asInstanceOf[Sampler[Spin]]
  val CDExamples = sequences.map(new ContrastiveDivergenceExampleVector(_, model, sampler, k))

  def trainAdaGradRDA() = {
    val optimizer = new AdaGradRDA(rate = learningRate, l1 = l1, l2 = l2, numExamples = sequences.length)
    val parameters = model.parameters
    parameters.keys.foreach(_.value) // make sure we initialize the values in a single thread
    optimizer.initializeWeights(parameters)

    val trainer = new ParallelOnlineTrainer(parameters, optimizer=optimizer, maxIterations=numIterations, logEveryN=0)
    trainer.replaceTensorsWithLocks()
    try {
      while (!trainer.isConverged) trainer.processExamples(CDExamples.shuffle)
    } finally {
      trainer.removeLocks()
      optimizer.finalizeWeights(parameters)
    }
  }

  trainAdaGradRDA()

  val frobeniusNorms = ConnectionStrengths(model)
  def strengths = frobeniusNorms

}


class MutualInformationConnectionPredictor(msa: MSA, excludeNeighbors: Boolean, neighbourhood:Int)
  extends ConnectionPredictor(msa, excludeNeighbors, neighbourhood) {

  private val sequences = msa.sequences
  private val d = msa.domain
  private val MIs = computeMIs()

  private def computeMIs() = {
    val freqModel = PottsModel.frequenciesAsWeights(sequences, d, excludeNeighbors, neighbourhood)

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

  def strengths = MIs
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
    val proposalDiff = new DiffList
    for(i <- 0 until k) { proposalDiff ++= sampler.processAll(contexts, returnDiffs=true )}
    //val proposalDiff = sampler.processAll(contexts, returnDiffs=true)
    model.factorsOfFamilyClass[DotFamily](proposalDiff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics, -1.0))
    proposalDiff.undo()
    model.factorsOfFamilyClass[DotFamily](proposalDiff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics))
  }
}

/*class WeightedContrastiveDivergenceExampleVector[C](val contexts: Iterable[C], val weight: Double, model: Model with Parameters, val sampler: Sampler[C], val k: Int = 1) extends Example {
  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
    require(gradient != null, "The ContrastiveDivergenceExample needs a gradient accumulator")
    //val proposalDiff = new DiffList
    //repeat(k) { proposalDiff ++= sampler.process(context) }
    val proposalDiff = sampler.processAll(contexts, returnDiffs=true)
    model.factorsOfFamilyClass[DotFamily](proposalDiff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics, -1.0*weight))
    proposalDiff.undo()
    model.factorsOfFamilyClass[DotFamily](proposalDiff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics, weight))
  }
}*/


//#####################################################
/*class WeightedContrastiveDivergenceConnectionPredictor(
  msa: MSA,
  excludeNeighbours: Boolean,
  neighbourhood: Int,
  potentialConnections: Set[(Int, Int)]=Set(),
  useLogFreqsAsLocalWeights: Boolean=true,
  numIterations: Int=3,
  learningRate: Double=0.1,
  l1: Double=0.01,
  l2: Double=0.000001,
  useParallelTrainer: Boolean=true)
  extends ConnectionPredictor(msa, excludeNeighbours, neighbourhood) {

  implicit val random = new scala.util.Random(0)

  val useAllConnections = (potentialConnections.size == 0)
  val model =
    if(useAllConnections) {
      PottsModel(msa, useLogFreqsAsLocalWeights, excludeNeighbours, neighbourhood)
    } else {
      PottsModel(msa,useLogFreqsAsLocalWeights, potentialConnections)
    }

  val sequences = msa.reweightedSequences
  val sampler = new GibbsSampler(model).asInstanceOf[Sampler[Spin]]
  val CDExamples = for((w, seq) <- sequences) yield new WeightedContrastiveDivergenceExampleVector(seq, w, model, sampler)
  val optimizer = new AdaGradRDA(rate=learningRate, l1 = l1, l2=l2, numExamples = msa.sequences.length)

  Trainer.onlineTrain(model.parameters, CDExamples,
    optimizer = optimizer, useParallelTrainer = useParallelTrainer, maxIterations = numIterations, logEveryN=0)

  val frobeniusNorms = ConnectionStrengths(model)
  def strengths = frobeniusNorms
}*/