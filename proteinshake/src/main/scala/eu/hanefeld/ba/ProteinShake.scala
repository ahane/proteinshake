package eu.hanefeld.ba
import cc.factorie.util.DefaultCmdOptions
import cc.factorie.variable.CategoricalDomain
import cc.factorie.util.{HyperParameter, LogUniformDoubleSampler, CmdOptions}
import eu.hanefeld.ba.Types._
import scala.concurrent.Future
import org.json4s.jackson.JsonMethods._
import org.json4s._
import scala.collection.mutable.{ListBuffer, ArrayBuffer}


class ProteinShakeTrainer extends cc.factorie.util.HyperparameterMain{
  object options extends ProteinShakeOptions

  def evaluateParameters(args: Array[String]): Double = {
    options.parse(args)

    val filePath = options.filePath.value
    val msa = ProteinShakeUtil.loadMSAFromFile(filePath)
    val tpRate = evaluateOneMSA(msa)
    tpRate
  }

  def evaluateOneMSA(msa: MSA): Double = {

    val doMISubselection = options.miSubselect.value
    val MIThreshold = options.miThreshold.value
    val excludeNeighbours = options.excludeNeighbours.value
    val neighbourhood = options.neighbourhood.value


    val topMIConnections: Set[(Int, Int)] =
      if(doMISubselection) {
        ProteinShakeUtil.MIConnectionsOverThreshold(msa, excludeNeighbours, neighbourhood, threshold=MIThreshold)
      } else {
        Set()
      }


    val l1 = options.l1.value
    val l2 = options.l2.value
    val lr = options.learningRate.value
    val iterations = options.numIterations.value
    val numSteps = options.cdSteps.value
    val useLogFreqs = options.useLogFreqs.value


    println("== Learning CD model ==")
    println("   Parameters: l1="+l1.toString+
      ", l2="+l2.toString+
      ", lr="+lr.toString +
      ", numIter="+iterations.toString+", " +
      "k="+numSteps.toString)

    if(doMISubselection){
      val allEdges = PottsModel.generatePairs(msa.sequences(0).length, excludeNeighbours, neighbourhood)
      val check = allEdges intersect topMIConnections
      println("   MI subselection passed "+topMIConnections.size+"/"+allEdges.size+" connections to CDPredictor")
    }

    val CDPred =
        new ContrastiveDivergenceConnectionPredictor(msa, potentialConnections=topMIConnections, l1=l1, l2=l2,
          learningRate=lr, numIterations=iterations, k=numSteps, excludeNeighbours=excludeNeighbours,
          neighbourhood=neighbourhood, useLogFreqs=useLogFreqs)

    println("   TPRate: " + CDPred.TPRate.toString)
    CDPred.saveResultsToFile(math.log(l1).toString+"_"+iterations.toString+"_"+numSteps.toString)
    CDPred.TPRate
  }


}

object ProteinShakeUtil {

  var currentMSA: MSA = null
  var currentFilepath: String = ""

  def loadMSAFromFile(filepath: String): MSA = {
    implicit val formats = DefaultFormats //for the json reader
    if(filepath == currentFilepath) currentMSA
    else {
      val json = parse(readFromFile(filepath))
      val msaJson = json.extract[MSAJson]
      currentMSA = MSA(msaJson)
      currentFilepath = filepath
      currentMSA
    }
  }

  private def readFromFile(filePath: String): String = {
    import scalax.io._
    try {
      val in: Input = Resource.fromFile(filePath)
      in.string
    }
    catch {
      case e: Exception => println("Failed to locate MSA data file");
        null
    }

  }

  var MIPredictor: MutualInformationConnectionPredictor = null
  def initSingletonMIPredictor(msa: MSA, excludeNeighbours: Boolean, neighbourhood: Int): MutualInformationConnectionPredictor = {
    MIPredictor = new MutualInformationConnectionPredictor(msa, excludeNeighbours, neighbourhood)
    MIPredictor.saveResultsToFile("MI")
    MIPredictor
  }

  def MIConnectionsOverThreshold(msa: MSA, excludeNeighbours: Boolean, neighbourhood: Int, threshold: Double=0.26): Set[(Int, Int)] = {

    val parametersDifferent = (MIPredictor.excludeNeighbours != excludeNeighbours) ||
                              (MIPredictor.msa != msa) ||
                              (MIPredictor.neighbourhood != neighbourhood)

    if(MIPredictor == null || parametersDifferent) {
      initSingletonMIPredictor(msa, excludeNeighbours, neighbourhood)
    }
    MIPredictor.connectionsWithStrengthsOfAtLeast(threshold)
  }

}

trait ProteinShakeOptions extends CmdOptions with DefaultCmdOptions{
  //
  val filePath = new CmdOption("msa-file", "data/synth/synth.json", "FILENAME", "Name of msa.json file that should be used.")

  val numIterations = new CmdOption("iterations", 3, "INT", "Number of iterations of the SGD algorithm")
  val useNumIter = new CmdOption("tune-iter", false, "BOOLEAN", "Set true if we want to vary the number of SGD iterations")

  val cdSteps = new CmdOption("cdsteps", 1, "INT", "Number of steps the CD sampler should take")
  val useCDSteps = new CmdOption("tune-cdsteps", false, "BOOLEAN", "Set true if we want to vary the number of sampling steps CD takes")


  val l1 = new CmdOption("l1", 0.0, "DOUBLE", "regularization rate l1")
  val l2 = new CmdOption("l2", 0.0, "DOUBLE", "regularization rate l2")
  val useL1 = new CmdOption("tune-l1", false, "BOOLEAN", "Set if l1 regularization should be used")
  val useL2 = new CmdOption("tune-l2", false, "BOOLEAN", "Set if l2 regularization should be used")


  val learningRate = new CmdOption("learning-rate", 0.9, "DOUBLE", "Learning rate")
  val useLearningRate = new CmdOption("tune-lr", false, "BOOLEAN", "Set if learning rate should be tuned")

  val numTrails = new CmdOption("trails", 5, "INT", "Number of hyperparamter trails to run")

  val excludeNeighbours = new CmdOption("exclude-neighbours", true, "BOOLEAN", "Set true if we want to ignore neighbouring sites")
  val neighbourhood = new CmdOption("neighbourshood", 4, "INT", "Set the distance of sites whose connectiosn will be ignored")


  val miSubselect = new CmdOption("mi-subselection", true, "BOOLEAN", "Set true if we only want to send connections with a minimum MI to the CDPredictor")
  val miThreshold = new CmdOption("mi-threshold", 0.2, "DOUBLE", "Minimum MI a connection should have to qualify for the CD predictor")

  val useLogFreqs =  new CmdOption("use-log-freqs", true, "BOOLEAN", "Set true if we want to initialize local weights with empirical log-frequencies")

}


object ProteinShakeOptimizer {
  object options extends ProteinShakeOptions

  def main(args: Array[String]) = {
    options.parse(args)
    import cc.factorie.util.{HyperParameter, HyperParameterSearcher, UniformDoubleSampler, SampleFromSeq}
    val iter = new HyperParameter(options.numIterations, new SampleFromSeq(Seq(1, 2, 3, 4, 5)))
    val l1  = cc.factorie.util.HyperParameter(options.l1, new LogUniformDoubleSampler(1e-12, 1))
    val l2  = cc.factorie.util.HyperParameter(options.l2, new LogUniformDoubleSampler(1e-12, 1))
    val lr  = cc.factorie.util.HyperParameter(options.learningRate, new LogUniformDoubleSampler(1e-3, 10))
    val k  = cc.factorie.util.HyperParameter(options.cdSteps, new SampleFromSeq(Seq(1, 2, 3, 5, 10)))


    //Why doesn't this find options.numTrails???
    val numTrails = options.numTrails.value
    val numToFinish = (numTrails * 0.7).toInt

    val optionsToSearch = new ListBuffer[HyperParameter[_]]
    if(options.useLearningRate.value) optionsToSearch.append(lr)
    if(options.useL1.value) optionsToSearch.append(l1)
    if(options.useL2.value) optionsToSearch.append(l2)
    if(options.useNumIter.value) optionsToSearch.append(iter)
    if(options.useCDSteps.value) optionsToSearch.append(k)

    val path = options.filePath.value
    val excludeNeighbours = options.excludeNeighbours.value
    val neighbourhood = options.neighbourhood.value
    val msa = ProteinShakeUtil.loadMSAFromFile(path)
    val MIPred = ProteinShakeUtil.initSingletonMIPredictor(msa, excludeNeighbours, neighbourhood)
    println("MI TP Rate: "+MIPred.TPRate.toString)

    val parameterSearcher = new HyperParameterSearcher(options, optionsToSearch, executor, numTrails, numToFinish)
    val optimalParameters = parameterSearcher.optimize()
    println(optimalParameters.toString)
  }

  def executor(cmdOptions: Array[String]): Future[Double] = {
    import concurrent.ExecutionContext.Implicits.global
    val f: Future[Double] = concurrent.future {
      val trainer = new ProteinShakeTrainer
      trainer.evaluateParameters(cmdOptions)
    }
    f onFailure {
      case t => println("An error has occured: " + t.getMessage)
    }

    f
  }
}
