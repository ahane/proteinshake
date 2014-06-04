package eu.hanefeld.ba

import cc.factorie.variable.{DoubleVariable, CategoricalDomain}
import scala.collection.mutable.ArrayBuffer
import cc.factorie.directed.{Gaussian, DirectedModel}
import cc.factorie.directed._
import cc.factorie.la.{DenseTensor2, DenseTensor1}

//import play.

/**
 * Created by alec on 3/23/14.
 */
class WeightGenerator(
  val domain: CategoricalDomain[Char],
  val numSites: Int,
  val edgeProbability: Double,
  //val loopFree: Boolean,
  val localMean: Double = 0,
  val localVariance: Double = 1.0,
  val pairwiseMean: Double = 0,
  val pairwiseVariance: Double = 3.0) {

  implicit val random = new scala.util.Random(0)

  //allEdges contains all permutations of our
  val allEdges = (for(i <- 0 until numSites; j <- 0 until numSites if i < j) yield (i, j)).toSet
  val activeEdges = drawFromSet(edgeProbability, allEdges)
  /*val activeEdges = if(loopFree) {
    drawFromSetLoopFree(edgeProbability, allEdges)
  } else {
    drawFromSet(edgeProbability, allEdges)
  }*/


  private def drawFromSet[A](p: Double, s: Set[A]) = {
    val drawn = for(i <- s if random.nextDouble < edgeProbability) yield i
    drawn
  }
  /* Do we really need this?
  def drawFromSetLoopFree(p: Double, s: Set[(Int, Int)]): Set[(Int, Int)] = {
    val drawn: ArrayBuffer[(Int, Int)] = new ArrayBuffer
    var already_connected: Set[Int] = Set()
    for(e <- s) {
      if(random.nextDouble < 0.3) {
        if(!((already_connected contains e._1) && (already_connected contains e._2))) {
          drawn += e
          already_connected += e._1
          already_connected += e._2
        }
      }
    }
    drawn.toSet
  }*/

  def generateLocalMasses: IndexedSeq[DenseTensor1] = {
    implicit val model = DirectedModel()

    val mean = new DoubleVariable(localMean)
    val variance = new DoubleVariable(localVariance)

    val localMasses = for(i <- 0 until numSites) yield new DenseTensor1(domain.size)
    for(massTensor <- localMasses){
      for(i <- 0 until massTensor.dim1){
        val mass: DoubleVariable = new DoubleVariable :~ Gaussian(mean, variance)
        massTensor(i) = mass.value
      }
    }
    localMasses
  }

  def generatePairwiseMasses: Map[(Int, Int), DenseTensor2] = {
    val pairMasses: Map[(Int, Int), DenseTensor2] =(for(e <- activeEdges) yield (e -> generatePairwiseMass)).toMap
    pairMasses
  }

  private def generatePairwiseMass: DenseTensor2 = {
    implicit val model = DirectedModel()

    val mean = new DoubleVariable(pairwiseMean)
    val variance = new DoubleVariable(pairwiseVariance)

    val massTensor = new DenseTensor2(domain.size, domain.size)
    for(a1 <- 0 until domain.size; a2 <- 0 until domain.size if a1 <= a2) {
      val mass: DoubleVariable = new DoubleVariable :~ Gaussian(mean, variance)
      //our masses are symmetric, as we only have one factor for (i, j) and (j, i)
      massTensor(a1, a2) = mass.value
      massTensor(a2, a1) = mass.value
    }
    massTensor
  }

}

object WeightGenerator {
  def apply(domain: CategoricalDomain[Char], numSites: Int, p: Double) = new WeightGenerator(domain, numSites, p)
  def apply(domain: CategoricalDomain[Char], p: Double) = new WeightGenerator(domain, 10, p)
}