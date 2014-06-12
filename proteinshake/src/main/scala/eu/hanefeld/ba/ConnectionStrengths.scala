package eu.hanefeld.ba

import cc.factorie.la.{Tensor, DenseTensor2}
import eu.hanefeld.ba.Types._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods.{render}
import scala.collection.mutable.ArrayBuffer
import org.json4s.JsonAST._
import org.json4s.Merge

/**
 * Created by alec on 3/23/14.
 */
class ConnectionStrengths(val strengths: Map[(Int, Int), Double], val sourceName: String, val source: Any = null){
  def apply(key: (Int, Int)): Double = strengths(key)
  val sortedByStrength = strengths.toList.sortBy(_._2).reverse

  def getPositivesAsJSON: JObject = {
    val jsons = new ArrayBuffer[JObject]
    for (connection <-strengths.keys) {
      val first = connection._1.toString
      val second = connection._2.toString
      val isConnection = JBool(strengths(connection) > 0.0)
      val jobject = JObject(
                        JField(first,
                               JObject( JField(second, isConnection) :: Nil ))
                         :: Nil)
      jsons.append(jobject)
    }
    val jsonMerged = jsons.reduceLeft((a: JObject, b: JObject) => Merge.merge(a, b))
    jsonMerged
  }

  //deprecated
  def getJSONFloat: JObject = {
    val jsons = new ArrayBuffer[JObject]
    for (connection <-strengths.keys) {
      val first = connection._1.toString
      val second = connection._2.toString
      val strength = JDouble(strengths(connection))
      val jobject = JObject(
        JField(first,
          JObject( JField(second, strength) :: Nil ))
          :: Nil)
      jsons.append(jobject)
    }
    val jsonMerged = jsons.reduceLeft((a: JObject, b: JObject) => Merge.merge(a, b))
    jsonMerged
  }

}
object ConnectionStrengths{

  def apply(model: PottsModel, name: String) = {
    val numSites = model.localFamilies.length
    val allEdges = (for(i <- 0 until numSites; j <- 0 until numSites if i < j) yield (i, j)).toSet

    val modelEdges = model.pairwiseFamilies.keys.toSet
    def modelWeights(edge: (Int, Int)): Tensor = model.pairwiseFamilies(edge).weights.value

    var strengths: Map[(Int, Int), Double] = Map()
    for(edge <- allEdges) {
      if(modelEdges contains edge) {
        strengths += (edge -> fNorm(modelWeights(edge)))
      }
      else strengths += (edge -> 0)
    }

    new ConnectionStrengths(strengths, name, model)
  }

  def apply(model: PottsModel): Map[(Int, Int), Double] = {
    val numSites = model.localFamilies.length
    val allEdges = (for(i <- 0 until numSites; j <- 0 until numSites if i < j) yield (i, j)).toSet

    val modelEdges = model.pairwiseFamilies.keys.toSet
    def modelWeights(edge: (Int, Int)): Tensor = model.pairwiseFamilies(edge).weights.value

    var strengths: Map[(Int, Int), Double] = Map()
    for(edge <- allEdges) {
      if(modelEdges contains edge) {
        strengths += (edge -> fNorm(modelWeights(edge)))
      }
      else strengths += (edge -> 0)
    }

    strengths
  }

  /*def MI(samples: Seq[SpinSequence], d: SpinDomain, name: String = "MI") = {
    val freqModel = PottsModel.frequenciesAsWeights(samples, d)
    val numSites = freqModel.localFamilies.length
    val allEdges = (for(i <- 0 until numSites; j <- 0 until numSites if i < j) yield (i, j)).toSet

    def localFreqs(i: Int) = freqModel.localFamilies(i).weights.value
    def pairwiseFreqs(i: Int, j: Int) = freqModel.pairwiseFamilies(i, j).weights.value.asInstanceOf[DenseTensor2]
    def singleMI(i: Int, j: Int): Double = {
      val f_i = localFreqs(i)
      val f_j = localFreqs(j)
      val f_ij = pairwiseFreqs(i, j)
      (for(k <- d.categories; l <- d.categories) yield (f_ij(d.index(k), d.index(l)) * math.log(f_ij(d.index(k), d.index(l)) / (f_i(d.index(k))*f_j(d.index(l)))))).sum
    }
    val allMIs: Map[(Int, Int), Double] = (for(edge <- allEdges) yield (edge, singleMI(edge._1, edge._2))).toMap
    new ConnectionStrengths(allMIs, name, freqModel)
  }

  def MI(samples: Seq[SpinSequence], d: SpinDomain) = {
    val freqModel = PottsModel.frequenciesAsWeights(samples, d)
    val numSites = freqModel.localFamilies.length
    val allEdges = (for(i <- 0 until numSites; j <- 0 until numSites if i < j) yield (i, j)).toSet

    def localFreqs(i: Int) = freqModel.localFamilies(i).weights.value
    def pairwiseFreqs(i: Int, j: Int) = freqModel.pairwiseFamilies(i, j).weights.value.asInstanceOf[DenseTensor2]
    def singleMI(i: Int, j: Int): Double = {
      val f_i = localFreqs(i)
      val f_j = localFreqs(j)
      val f_ij = pairwiseFreqs(i, j)
      (for(k <- d.categories; l <- d.categories) yield (f_ij(d.index(k), d.index(l)) * math.log(f_ij(d.index(k), d.index(l)) / (f_i(d.index(k))*f_j(d.index(l)))))).sum
    }
    val allMIs: Map[(Int, Int), Double] = (for(edge <- allEdges) yield (edge, singleMI(edge._1, edge._2))).toMap
    allMIs
  }
 */
  private def fNorm(w: Tensor): Double = {
    val t = w.asInstanceOf[DenseTensor2]
    def averageOverRow(t: DenseTensor2, k: Int): Double = {
      // J_ij(k, .) k being a row
      val row = for(l <- 0 until t.dim2) yield t(k, l)
      row.sum/row.length
    }
    def averageOverCol(t: DenseTensor2, l: Int): Double = {
      // J_ij(., l) l being a column
      val col = for(k <- 0 until t.dim1) yield t(k, l)
      col.sum/col.length
    }
    val totalAverage: Double = t.sum/t.length
    val transformed: DenseTensor2 = t.copy
    for(k <- 0 until t.dim1; l <- 0 until t.dim2) {
      transformed(k, l) = t(k, l) - averageOverRow(t, k) - averageOverCol(t, l) + totalAverage
    }
    math.sqrt(transformed.twoNorm)
  }
}
