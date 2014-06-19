package eu.hanefeld.ba
import cc.factorie.variable.CategoricalDomain
import cc.factorie.util.{HyperParameter, LogUniformDoubleSampler, CmdOptions}
import eu.hanefeld.ba.Types._
import scala.concurrent.Future
import org.json4s.jackson.JsonMethods._
import org.json4s._
import scala.collection.mutable.{ListBuffer, ArrayBuffer}


object ProteinShakeTrainerSingle extends ProteinShakeTrainer

class ProteinShakeTrainer extends cc.factorie.util.HyperparameterMain{
  object options extends ProteinShakeOptions

  def evaluateParameters(args: Array[String]): Double = {
    options.parse(args)
    val msaFoldername = "data/" + options.dataPath.value
    val msaFilename = options.msa.value
    val tpRate = evaluateOneMSA(msaFoldername, msaFilename)
    tpRate
  }

  def evaluateOneMSA(foldername: String, filename: String): Double = {

    val path = foldername + filename
    val json = parse(readFromFile(path))

    implicit val formats = DefaultFormats //for the json reader
    val msaJson = json.extract[MSAJson]
    val msa = MSA(msaJson)
    val MIpred = new MututalInformationConnectionPredictor(msa, true)
    println("MI TP Rate: " + MIpred.TPRate.toString)
    val doMISubselection = options.miSubselect.value
    //this threshold (0.26) comes from [Weigt08]
    val topMIConnections: Set[(Int, Int)] = if(doMISubselection) MIpred.connectionsWithStrengthsOfAtLeast(0.26) else Set()
    println("--learning CD model--")

    val l1 = options.l1.value
    val l2 = options.l2.value
    val lr = options.learningRate.value
    val iterations = options.numIterations.value
    val numSteps = options.cdSteps.value
    val useLogFreqs = options.useLogFreqs.value
    println("with parameters: l1="+l1.toString+
      ", l2="+l2.toString+
      ", lr="+lr.toString +
      ", numIter="+iterations.toString+", " +
      "k="+numSteps.toString)

    val CDPred =
        new ContrastiveDivergenceConnectionPredictor(msa, potentialConnections=topMIConnections, l1=l1, l2=l2,
          learningRate=lr, numIterations=iterations, k=numSteps, excludeNeighbours=true, useLogFreqs=useLogFreqs)

    //val cdpred = new MututalInformationConnectionPredictor(msa, true)
    println("TPRate: " + CDPred.TPRate.toString)
    CDPred.saveResultsToFile(l1.toString+"_"+iterations.toString+"_"+numSteps.toString)
    CDPred.TPRate
  }

  private def readFromFile(filepath: String): String = {
    import scalax.io._
    try {
      val in: Input = Resource.fromFile(filepath)
      return in.string
    }
    catch {
      case e: Exception => println("Failed to locate MSA data file");
      return None.asInstanceOf[String]
    }

  }
}

trait ProteinShakeOptions extends CmdOptions {
  //
  val dataPath = new CmdOption("msa-folder", "synth/", "STRING", "Path to MSA files")
  val msa = new CmdOption("msa-file", "synth.json", "STRING", "Name of msa.json file that should be used.")

  val numIterations = new CmdOption("iterations", 3, "INT", "Number of iterations of the SGD algorithm")
  val useNumIter = new CmdOption("use-iter", false, "BOOLEAN", "Set true if we want to vary the number of SGD iterations")

  val cdSteps = new CmdOption("cdsteps", 3, "INT", "Number of steps the CD sampler should take")
  val useCDSteps = new CmdOption("use-cdsteps", false, "BOOLEAN", "Set true if we want to vary the number of sampling steps CD takes")


  val l1 = new CmdOption("l1", 0.0, "DOUBLE", "regularization rate l1")
  val l2 = new CmdOption("l2", 0.0, "DOUBLE", "regularization rate l2")
  val useL1 = new CmdOption("use-l1", false, "BOOLEAN", "Set if l1 regularization should be used")
  val useL2 = new CmdOption("use-l2", false, "BOOLEAN", "Set if l2 regularization should be used")

  val learningRate = new CmdOption("learning-rate", 0.1, "DOUBLE", "Learning rate")

  val numTrails = new CmdOption("trails", 5, "INT", "Number of hyperparamter trails to run")

  val miSubselect = new CmdOption("MI-subselection", true, "BOOLEAN", "Set true if we only want to send connections with a minimum MI to the CDPredictor")
  val useLogFreqs =  new CmdOption("--use-log-freqs", true, "BOOLEAN", "Set true if we want to initialize local weights with empirical log-frequencies")

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

    val optionsToSearch = ListBuffer[HyperParameter[_]](lr)
    if(options.useL1.value) optionsToSearch.append(l1)
    if(options.useL2.value) optionsToSearch.append(l2)
    if(options.useNumIter.value) optionsToSearch.append(iter)
    if(options.useCDSteps.value) optionsToSearch.append(k)


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
    f
  }
}
