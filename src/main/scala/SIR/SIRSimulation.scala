package SIR

import java.io.FileWriter

import graph.SimGraph
import scalax.collection.GraphEdge.UnDiEdge

import scala.collection.Seq

class SIRSimulation(filename: String, nodes: Seq[SIRNode], edges: Seq[UnDiEdge[SIRNode]]) {
  private val simGraph = new SimGraph[SIRNode](nodes, edges)
  val fw = new FileWriter(filename, false)
  private val headers = "TIME" +: SIRCompartments.values.toList.sortBy(_.id).map(_.toString)
  fw.write(headers.mkString(",") + "\n")

  def next(): Boolean = {
    val stateCount = simGraph.getGraph.nodes.toIndexedSeq
      .map(innerNode => innerNode.toOuter)
      .groupBy(sirNode => sirNode.state.compartment)
    val newLine = simGraph.getTime.toString +: SIRCompartments.values.toList.sortBy(_.id).map(st =>
      if (!stateCount.keySet.contains(st)) 0
      else stateCount(st).size
    )
    fw.write(newLine.mkString(",") + "\n")
    simGraph.next()
  }
}
