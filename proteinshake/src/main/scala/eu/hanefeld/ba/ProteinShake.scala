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
  }
  def evaluateParameters(args : Array[String]): Double = {
    implicit val formats = DefaultFormats
    Options.parse(args)

    val msaFoldername = "data/" + Options.dataPath.value
    val msaFilename = Options.msa.value
    val path = msaFoldername + msaFilename
    val json = parse(readFromFile(path))
    val msaJson = json.extract[MSAJson]
    val msa = MSA(msaJson)
    val over26MI = Set((1,20), (7,21), (1,8), (4,26), (2,5), (16,22), (2,28), (19,27), (0,28), (7,12), (0,29), (3,11), (1,5), (7,9), (17,26), (16,27), (9,10), (15,27), (16,21), (8,10), (18,23), (8,27), (25,28), (2,17), (4,12), (13,23), (26,29), (14,19), (7,20), (19,25), (6,7), (8,9), (10,12), (15,21), (0,17), (13,25), (12,25), (5,15), (18,19), (12,23), (3,9), (1,21), (2,27), (24,28), (6,24), (20,24), (0,2), (21,23), (10,25), (0,25), (9,25), (13,19), (14,27), (3,10), (10,19), (22,26), (25,26), (13,28), (26,28), (2,19), (0,19), (3,25), (6,14), (5,13), (1,28), (16,20), (1,29), (4,10), (0,22), (4,17), (3,4), (22,23), (24,25), (22,24), (14,16), (27,29), (7,24), (7,23), (13,17), (10,20), (0,10), (5,19), (23,24), (11,19), (8,19), (25,29), (9,19), (4,7), (9,23), (2,10), (6,22), (2,24), (0,20), (24,27), (0,18), (7,19), (11,24), (1,24), (26,27), (19,21), (10,28), (17,20), (24,26), (20,22), (13,15), (0,9), (12,20), (5,11), (17,23), (19,24), (18,22), (8,18), (7,8), (8,26), (9,18), (8,29), (5,9), (21,28), (17,29), (7,16), (1,18), (21,24), (11,14), (19,23), (3,13), (0,7), (2,13), (0,3), (3,22), (6,9), (4,28), (5,20), (23,27), (8,11), (20,26), (7,26), (7,29), (12,18), (16,18), (19,29), (8,17), (7,22), (5,17), (5,10), (14,18), (9,13), (1,6), (1,11), (0,5), (3,6), (6,23), (19,22), (2,8), (1,19), (20,28), (8,13), (5,29), (17,25), (11,16), (6,8), (6,27), (1,12), (3,28), (6,15), (3,27), (6,11), (11,22), (12,29), (3,5), (0,15), (23,29), (14,28), (7,14), (8,25), (10,26), (20,23), (21,26), (14,24), (1,10), (3,24), (4,29), (10,23), (3,18), (9,28), (13,24), (4,6), (22,28), (8,23), (6,13), (20,29), (4,20), (4,11), (1,9), (3,12), (7,13), (4,13), (1,25), (8,24), (2,15), (11,29), (27,28), (4,5), (17,21), (18,28), (13,22), (6,16), (1,4), (12,16), (2,6), (17,22), (10,13), (0,8), (8,20), (4,18), (9,15), (13,18), (8,15), (11,21), (4,9), (0,4), (22,27), (12,15), (0,23), (1,27), (2,9), (2,29), (10,16), (5,7), (12,13), (23,26), (11,26), (21,29), (4,27), (5,12), (12,22), (15,16), (11,17), (13,29), (11,25), (7,11), (18,24), (19,20), (3,21), (8,16), (10,14), (5,16), (6,28), (1,16), (21,22), (9,27), (7,10), (0,12), (0,16), (6,19), (7,17), (12,26), (11,23), (2,21), (0,13), (24,29), (0,27), (1,3), (1,15), (6,21), (4,8), (5,28), (4,23), (2,14), (10,11), (2,23), (2,7), (5,21), (14,15), (9,14), (11,27), (6,18), (9,26), (2,4), (11,13), (4,25), (1,22), (3,7), (14,23), (1,14), (0,11), (8,21), (9,21), (1,26), (4,24), (7,27), (11,18), (12,19), (6,17), (9,17), (15,19), (18,27), (16,19), (9,22), (15,23), (0,1), (15,24), (0,21), (17,24), (20,21), (13,26), (11,28), (16,26), (13,21), (4,14), (2,22), (18,25), (7,28), (1,23), (1,17), (18,29), (8,22), (12,21), (1,13), (4,15), (22,29), (2,11), (5,25), (5,8), (21,25), (2,16), (6,20), (6,12), (5,22), (1,7), (19,26), (0,26), (23,28), (3,19), (16,23), (6,26), (2,3), (5,18), (23,25), (4,22), (2,20), (13,20), (11,15), (1,2), (5,23), (15,18), (10,22), (3,16), (25,27), (3,14), (16,25), (3,17), (7,25), (8,12), (17,27), (9,24), (21,27), (13,27), (15,26), (12,24), (11,20), (3,26), (2,26), (28,29), (14,29), (18,21), (15,20), (13,14), (17,18), (14,21), (11,12), (20,25), (18,26), (14,22), (5,14), (9,12), (9,20), (18,20), (17,19), (3,8), (19,28), (5,27), (8,28), (13,16), (12,28), (4,19), (5,6), (22,25), (14,20), (20,27), (16,28), (2,25), (4,16), (12,14), (0,6), (5,24), (16,29), (5,26), (10,27), (7,18), (14,26), (2,12), (0,24), (3,20), (2,18), (8,14), (12,27), (9,29), (6,29), (6,25), (4,21), (15,29), (3,23), (9,11))
    println("--learning CD model--")
    val l1 = Options.l1.value
    val l2 = Options.l2.value
    val lr = Options.learningRate.value
    val iterations = Options.numIterations.value
    println("with parameters: l1="+l1.toString+", l2="+l2.toString+", lr="+lr.toString)
    val cdpred = new ContrastiveDivergenceConnectionPredictor(msa, l1=l1, l2=l2, learningRate=lr)
    println("TPRate: " + cdpred.TPRate.toString)
    cdpred.TPRate


//  import options._
//  println(numIterations.value)
//  println(l1.value)
//  println(l1.value.toInt*numIterations.value.toInt)
//
    //println(hyperparameterTuning.optimalParameters)
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
}

object ProteinShakeOptimizer {
  def main(args: Array[String]) = {

    val options = ProteinShakeTrainer.Options
    options.parse(args)
    import cc.factorie.util.{HyperParameter, HyperParameterSearcher, UniformDoubleSampler, SampleFromSeq}
    //val numIterationsParam = new HyperParameter(options.numIterations, new SampleFromSeq(Seq(1, 2, 3, 4, 5)))
    val l1 = cc.factorie.util.HyperParameter(options.l1, new LogUniformDoubleSampler(1e-12, 1))
    val l2 = cc.factorie.util.HyperParameter(options.l2, new LogUniformDoubleSampler(1e-12, 1))
    val lr = cc.factorie.util.HyperParameter(options.learningRate, new LogUniformDoubleSampler(1e-3, 10))



    //val qs = new cc.factorie.util.QSubExecutor(10, "cc.factorie.app.nlp.ner.BasicConllNerTrainer")
    val parameterSearcher = new HyperParameterSearcher(options, List( l1, l2, lr), executor, 50, 10)
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
