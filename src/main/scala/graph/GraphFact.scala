package graph

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._

object GraphFact {
  def meanDegree[NodeType <: Node](graph: Graph[NodeType, UnDiEdge]): Double = {
    val sumDegree: Int = graph.nodes.toIndexedSeq.map(_.neighbors.size).sum
    sumDegree.toDouble / graph.nodes.size
  }
}
