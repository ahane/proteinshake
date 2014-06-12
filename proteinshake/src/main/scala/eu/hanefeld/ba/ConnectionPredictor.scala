package eu.hanefeld.ba

import cc.factorie.GibbsSampler
import cc.factorie.infer.Sampler
import cc.factorie.optimize.{AdaGrad, AdaGradRDA, Trainer}
import cc.factorie.la.DenseTensor2
import eu.hanefeld.ba.Types.SpinValue

/**
 * Created by alec on 6/10/14.
 */
abstract class ConnectionPredictor(val msa: MSA) {

  def strengths: Map[(Int, Int), Double]

  def positives: Set[(Int, Int)] = msa.connections

  def strengthsDescending: List[((Int, Int), Double)] = { strengths.toList.sortBy(_._2).reverse }

  def TPRate(upUntil: Int): Double = {
    val numPositives = positives.size
    val until = if(upUntil > numPositives) numPositives else upUntil
    val predicted = strengthsDescending.slice(0, until).map(_._1).toSet
    val truePositives = positives intersect predicted
    val tpRate = truePositives.size / numPositives
    tpRate
  }
  def TPRate: Double = { TPRate(positives.size) }
}

class ContrastiveDivergenceConnectionPredictor(
  msa: MSA,
  useLogFreqsAsLocalWeights: Boolean=true,
  numIterations: Int=3,
  l1: Double=0.5,
  useParallelTrainer: Boolean=true,
  opt: String="AdaGradRDA") extends ConnectionPredictor(msa) {

  implicit val random = new scala.util.Random(0)

  val sequences = msa.sequences
  val model = PottsModel(msa, useLogFreqsAsLocalWeights, excludeNeighbors=true)
  val sampler = new GibbsSampler(model).asInstanceOf[Sampler[Spin]]
  val CDExamples = sequences.map(new ContrastiveDivergenceExampleVector(_, model, sampler))
  val optimizer = opt match {
    case "AdaGrad" =>  new AdaGrad
    case "AdaGradRDA" =>  new AdaGradRDA(l1 = l1)
    case _ => new AdaGradRDA(l1 = l1)
  }

  Trainer.onlineTrain(model.parameters, CDExamples,
    optimizer = optimizer, useParallelTrainer = useParallelTrainer, maxIterations=numIterations, logEveryN=200)

  def strengths = ConnectionStrengths(model)
}

class MututalInformationConnectionPredictor(msa: MSA) extends ConnectionPredictor(msa) {

  val sequences = msa.sequences
  val d = msa.domain
  def strengths = {
    //we abuse the PottsModel infrastructure for convenient counting of pairwise and single-site frequencies
    val freqModel = PottsModel.frequenciesAsWeights(sequences, d)

    def localFreqs(i: Int) = freqModel.localFamilies(i).weights.value
    def pairwiseFreqs(i: Int, j: Int) = freqModel.pairwiseFamilies(i, j).weights.value

    def singleMI(i: Int, j: Int): Double = {
      println("Calculation MI("+i.toString+","+j.toString+")")
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

    val allEdges = freqModel.pairwiseFamilies.keys
    val allMIs: Map[(Int, Int), Double] = (for(edge <- allEdges) yield (edge, singleMI(edge._1, edge._2))).toMap
    allMIs
  }
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
