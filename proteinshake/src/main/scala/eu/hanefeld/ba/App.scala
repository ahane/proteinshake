package eu.hanefeld.ba
import cc.factorie.variable.CategoricalDomain

object App {
  
  def foo(x : Array[String]) = x.foldLeft("")((a,b) => a + b)
  
  def main(args : Array[String]) {
    val d = new CategoricalDomain[Char](List('a', 'b'))
    //val s = Spin.vector("aaa", d)

    println(d.size)
    println("hooo")
  }

}
