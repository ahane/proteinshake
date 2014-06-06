package eu.hanefeld.ba
import cc.factorie.variable.CategoricalDomain
import cc.factorie.util.CmdOptions
import eu.hanefeld.ba.Types._
import scala.concurrent.Future


object App {

  val aminoDomain: SpinDomain = new CategoricalDomain[Char](List('a', 'b', 'c'))
  val MSAGen = MSAGenerator(20, aminoDomain, "test1")
  def main(args : Array[String]) {
//  options.parse(args)
//  import options._
//  println(numIterations.value)
//  println(l1.value)
//  println(l1.value.toInt*numIterations.value.toInt)
//
    println(hyperparameterTuning.optimalParameters)
  }

}

object options extends CmdOptions {
  //
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