package eu.hanefeld.ba
import cc.factorie.variable.CategoricalDomain
import eu.hanefeld.ba.Types._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import scalax.io.{Resource, Output}
import java.io.FileOutputStream

object App {

  val aminoDomain: SpinDomain = new CategoricalDomain[Char](List('a', 'b', 'c'))
  val MSAGen = MSAGenerator(20, aminoDomain, "test1")
  def main(args : Array[String]) {
    MSAGen.generateSequenceStrings(5)
    val out: Output = Resource.fromOutputStream(new java.io.FileOutputStream("test1.dat"))
    out.write(MSAGen.getJSON)
    //print(compact(render(json)))
    //println(MSAGen.getJSON())
    println("hooo")
  }

}
