package eu.hanefeld.ba
import cc.factorie.variable.CategoricalDomain
import cc.factorie.util.{LogUniformDoubleSampler, CmdOptions}
import eu.hanefeld.ba.Types._
import scala.concurrent.Future
import org.json4s.jackson.JsonMethods._
import org.json4s._



object ProteinShakeTrainer extends cc.factorie.util.HyperparameterMain{
  object Options extends CmdOptions {
    //
    val dataPath = new CmdOption("msa-folder", "synth/", "STRING", "Path to MSA files")
    val msa = new CmdOption("msa-file", "synth.json", "STRING", "Name of msa.json file that should be used.")
    val numIterations = new CmdOption("iterations", 3, "INT", "Number of iterations of the SGD algorithm")
    val l1 = new CmdOption("l1", 0.01, "DOUBLE", "Learning rate l1")
    val l2 = new CmdOption("l2", 0.00001, "DOUBLE", "Learning rate l1")
    val learningRate = new CmdOption("learning-rate", 0.5, "DOUBLE", "Learning rate l1")
    val numTrails = new CmdOption("trails", 5, "INT", "Number of hyperparamter trails to run")
    val miSubselect = new CmdOption("MI-subselection", true, "BOOL", "Set true if we only want to send connections with a minimum MI to the CDPredictor")
  }
  def evaluateParameters(args : Array[String]): Double = {

    Options.parse(args)

    val msaFoldername = "data/" + Options.dataPath.value
    val msaFilename = Options.msa.value
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
    val doMISubselection = Options.miSubselect.value
    //this threshold (0.26) comes from [Weigt08]
    val topMIConnections: Set[(Int, Int)] = if(doMISubselection) MIpred.connectionsWithStrengthsOfAtLeast(0.26) else Set()
    println("--learning CD model--")
    val l1 = Options.l1.value
    val l2 = Options.l2.value
    val lr = Options.learningRate.value
    val iterations = Options.numIterations.value
    println("with parameters: l1="+l1.toString+", l2="+l2.toString+", lr="+lr.toString)
    val cdpred = new ContrastiveDivergenceConnectionPredictor(msa, potentialConnections=topMIConnections, l1=l1, l2=l2, learningRate=lr, numIterations=iterations)
    //val cdpred = new MututalInformationConnectionPredictor(msa, true)
    println("TPRate: " + cdpred.TPRate.toString)
    cdpred.TPRate
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

object Options extends CmdOptions {
  //

  val dataPath = new CmdOption("msa-folder", "synth/", "STRING", "Path to MSA files")
  val msa = new CmdOption("msa-file", "synth.json", "STRING", "Name of msa.json file that should be used.")
  val numIterations = new CmdOption("iterations", 3, "INT", "Number of iterations of the SGD algorithm")
  val l1 = new CmdOption("l1", 0.01, "DOUBLE", "Learning rate l1")
  val l2 = new CmdOption("l2", 0.00001, "DOUBLE", "Learning rate l1")
  val learningRate = new CmdOption("learning-rate", 0.5, "DOUBLE", "Learning rate l1")
  //

}

object ProteinShakeOptimizer {
  def main(args: Array[String]) = {

    val options = ProteinShakeTrainer.Options
    options.parse(args)
    import cc.factorie.util.{HyperParameter, HyperParameterSearcher, UniformDoubleSampler, SampleFromSeq}
    val iter = new HyperParameter(options.numIterations, new SampleFromSeq(Seq(1, 2, 3, 4, 5)))
    val l1 = cc.factorie.util.HyperParameter(options.l1, new LogUniformDoubleSampler(1e-12, 1))
    val l2 = cc.factorie.util.HyperParameter(options.l2, new LogUniformDoubleSampler(1e-12, 1))
    val lr = cc.factorie.util.HyperParameter(options.learningRate, new LogUniformDoubleSampler(1e-3, 10))

    //Why doesn't this find options.numTrails???
    val numTrails = options.numTrails.value
    val numToFinish = (numTrails * 0.7).toInt

    val parameterSearcher = new HyperParameterSearcher(options, List(iter, l1, l2, lr), executor, numTrails, numToFinish)
    val optimalParameters = parameterSearcher.optimize()
    println(optimalParameters.toString)
  }
  def executor(cmdOptions: Array[String]): Future[Double] = {
    import concurrent.ExecutionContext.Implicits.global
    val f: Future[Double] = concurrent.future {
      ProteinShakeTrainer.evaluateParameters(cmdOptions)
    }
    f
  }
}
