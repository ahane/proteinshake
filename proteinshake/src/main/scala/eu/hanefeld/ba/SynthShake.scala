package eu.hanefeld.ba

import eu.hanefeld.ba.Types._
import cc.factorie.variable.CategoricalDomain
import cc.factorie.util.CmdOptions

//import scalax.io.Resource
/**
 * Created by alec on 6/6/14.
 */
object SynthShake {

  def main(args: Array[String]) {
    SynthOptions.parse(args)

    val numSites = SynthOptions.numSites.value
    val domainSize = SynthOptions.domainSize.value
    val name = SynthOptions.name.value
    val edgeProbability = SynthOptions.edgeProbability.value
    val numSamples = SynthOptions.numSamples.value
    createAndSaveMSA(numSites, domainSize, name, edgeProbability, numSamples)

  }

  def createAndSaveMSA(numSites: Int, domainSize: Int, name: String, edgeProbability: Double, numSamples: Int): Unit = {
    val domain = makeDomain(domainSize)
    println("Intantiating MSA Generator...")
    val generator = MSAGenerator(numSites, domain, name, edgeProbability)
    println("Drawing samples...")
    generator.generateSequenceStrings(numSamples)
    println("Saving to file...")
    saveToFile(name, generator.getJSON)
    println("Done.")

  }

  private def saveToFile(name: String, json: String): Unit = {
    import scalax.io._

    val out: Output = Resource.fromFile("data/syn/"+ name + ".json" )
    out.write(json)
  }
  private def makeDomain(domainSize: Int): SpinDomain = {
    assert(domainSize <= 21, "Domains bigger than 21 are not supported.")
    val AMINOACIDS = Set('a', 'r', 'n', 'd', 'c', 'q', 'e', 'g', 'h', 'i', 'l', 'k', 'm', 'f', 'p', 's', 't', 'w', 'y', 'v')
    val activeValues = AMINOACIDS.slice(from=0, until=domainSize)
    val activeValuesGap = activeValues.union(Set('-'))
    val domain: SpinDomain = new CategoricalDomain[Char](activeValuesGap)
    domain
  }
}

object SynthOptions extends CmdOptions {
  val numSites = new CmdOption("num-sites", 50, "INT", "Number of spins in this model")
  val domainSize = new CmdOption("domain-size", 21, "INT", "Number of different values a site can take")
  val name = new CmdOption("name", "synth", "STRING", "Name of the synthetic MSA to make")
  val edgeProbability = new CmdOption("edge-prob", 0.4, "DOUBLE", "Probability of a connection between two sites existing")
  val numSamples = new CmdOption("num-samples", 1000, "INT", "Number of sequences to generate")

  //Commands for the SynthShakeSeries App
  val numMSAs = new CmdOption("num-msa", 10, "INT", "Number of sythetic MSAs to generate")
  val shouldSample = new CmdOption("sample-edge-probs", false, "BOOL", "Set to true if a synthetic MSA series should uniformly sample an edge probability for each series")
  val maxEdgeProb = new CmdOption("max-edge-prob", 0.5, "DOUBLE", "Defines the upper bound in case the edge probabilities are sampled.")
}

object SynthShakeSeries {
  def main(args: Array[String]) = {
    SynthOptions.parse(args)

    val numSites = SynthOptions.numSites.value
    val domainSize = SynthOptions.domainSize.value
    val nameStem = SynthOptions.name.value
    val numSamples = SynthOptions.numSamples.value

    val numMSAs = SynthOptions.numMSAs.value
    for(i <- 0 until numMSAs) {
      println("Creating MSA no. " + i.toString + " of " + numMSAs.toString)
      val name = nameStem + i.toString
      SynthShake.createAndSaveMSA(numSites, domainSize, name, this.edgeProbability, numSamples)
    }
  }

  def edgeProbability: Double = {
    val shouldSample = SynthOptions.shouldSample.value
    if(shouldSample) {
      val max = SynthOptions.maxEdgeProb.value
      assert(max <= 1, "Edge probabilities can't be bigger than 1")
      val random = scala.util.Random
      val draw = random.nextDouble()
      val probability = draw*max
      probability
    }
    else {
      SynthOptions.edgeProbability.value
    }
  }
}
