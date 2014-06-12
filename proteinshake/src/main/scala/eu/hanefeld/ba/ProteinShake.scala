package eu.hanefeld.ba
import cc.factorie.variable.CategoricalDomain
import cc.factorie.util.CmdOptions
import eu.hanefeld.ba.Types._
import scala.concurrent.Future
import org.json4s.jackson.JsonMethods._
import org.json4s._



object ProteinShake {

  def main(args : Array[String]) {
    implicit val formats = DefaultFormats
    options.parse(args)

    val msaFoldername = options.dataPath.value
    val msaFilename = options.msa.value
    val path = msaFoldername + msaFilename
    val json = parse(readFromFile(path))
    val msaJson = json.extract[MSAJson]
    val msa = MSA(msaJson)

    println("--calculating mututal information--")
    val mipred = new MututalInformationConnectionPredictor(msa)
    println("TPRate:" + mipred.TPRate.toString)
    println()
    println("--learning CD model--")
    val cdpred = new ContrastiveDivergenceConnectionPredictor(msa)
    println("TPRate: " + cdpred.TPRate.toString)



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

object options extends CmdOptions {
  //
  val dataPath = new CmdOption("msa-folder", "data/real/", "STRING", "Path to MSA files")
  val msa = new CmdOption("msa-file", "PF00006.json", "STRING", "Name of msa.json file that should be used.")
  val numIterations = new CmdOption("iterations", 3, "INT", "Number of iterations of the SGD algorithm")
  val l1 = new CmdOption("learning-rate", 0.4, "DOUBLE", "Learning rate l1")
}

object hyperparameterTuning {
  import cc.factorie.util.{HyperParameter, HyperParameterSearcher, UniformDoubleSampler, SampleFromSeq}
  val numIterationsParam = new HyperParameter(options.numIterations, new SampleFromSeq(Seq(1, 2, 3, 4, 5)))
  val l1Param = new HyperParameter(options.l1, new UniformDoubleSampler(0, 1))

  import concurrent.ExecutionContext.Implicits.global

  //val executor = (a: Array[String]) => concurrent.future { 1.0 }
  val parameterSearcher = new HyperParameterSearcher(options, List(numIterationsParam, l1Param), computeObjectiveFunction, 10, 5)
  val optimalParameters = parameterSearcher.optimize()



  def computeObjectiveFunction(cmdOptions: Array[String]): Future[Double] = {
    val f: Future[Double] = concurrent.future {
      options.parse(cmdOptions)
      val obj = options.numIterations.value*options.l1.value
      obj
    }
    f
  }
}
