package SIR

import graph.GraphGen
import scalax.collection.GraphEdge.UnDiEdge

import scala.collection.Seq

object Main {
  // currying makes stuff quite easy
  def makeSimulation(numNodes: Int, numSeed: Int, logFilename: String)
                    (infectionRate: Double, recoveryRate: Double)
                    (graphGen: Seq[SIRNode]=>Seq[UnDiEdge[SIRNode]]): SIRSimulation = {
    val nodes = (0 until numNodes).map(i=>new SIRNode(i, new SIRState(SIRCompartments.SUSCEPTIBLE), infectionRate, recoveryRate))
    // randomly pick numSeed of them to infect
    val seedIndices = utils.Sampler.sampleDiscreteUniform(0, numNodes, excluded = Set(), sampleSize = numSeed)
    for (seedIdx <- seedIndices) { // flip these guys
      nodes(seedIdx).state.compartment = SIRCompartments.INFECTED
    }
    val edges = graphGen(nodes)
    new SIRSimulation(logFilename, nodes, edges)
  }

  def main(args: Array[String]): Unit = {
    val numNodes = 5000
    val numSeed = 10
    val expectedNumNeighbors = 10
    val aggInfectionRate: Double = 0.56
    val individualInfectionRate = aggInfectionRate / expectedNumNeighbors

    // determine parameter set below
    // 1. Erdos-Renyi
//    val erdosProb: Double = expectedNumNeighbors.toDouble / numNodes
//    val graphGen = {nodes: Seq[SIRNode] => GraphGen.erdosRenyi[SIRNode](nodes, erdosProb)}
//    val sim = makeSimulation(numNodes, numSeed, s"out/SIR_ER_${numNodes/1000}k.csv")(individualInfectionRate, 1/4.1)(graphGen)

    // 2. Watts-Strogatz
    val wsProb = 0.05
    val graphGen = {nodes: Seq[SIRNode] => GraphGen.wattsStrogatz[SIRNode](nodes, expectedNumNeighbors, wsProb)}
    val sim = makeSimulation(numNodes, numSeed, s"out/SIR_WS_${numNodes/1000}k_p$wsProb.csv")(individualInfectionRate, 1/4.1)(graphGen)

    var iter = 0
    do {
      println(s"ITER: $iter")
      iter += 1
    } while(sim.next() && iter < numNodes * 2)
    sim.fw.flush()
    sim.fw.close()
  }
}
