package graph

import scalax.collection.Graph
import scalax.collection.GraphEdge._
import utils.Sampler

import scala.collection.{mutable, Seq}

class SimGraph[NodeType <: Node](nodes: Seq[NodeType], edges: Seq[UnDiEdge[NodeType]]) {
  private var time: Double = 0.0
  def getTime: Double = time

  // print out some graph statistics will be nice
  private val graph: Graph[NodeType, UnDiEdge] = Graph.from(nodes, edges)
  println(s"Num Nodes: ${graph.nodes.size}")
  println(s"Num Edges: ${graph.edges.size}")
  println(s"Average Node Degree: ${GraphFact.meanDegree(graph)}")

  def getGraph: Graph[NodeType, UnDiEdge] = graph

  // update node state & return time when next event happens
  def next(): Boolean = {
    val eventRates: mutable.Map[NodeType, Double] = mutable.Map()
    for (innerNode <- graph.nodes) {
      val outerNode = innerNode.toOuter
      val nodeEventRate = outerNode.eventRate(innerNode.neighbors.map(_.toOuter))
      nodeEventRate match {
        case Some(value) => eventRates(outerNode) = value
        case None => // uneventful node...
      }
    }
    val totalEventRate = eventRates.values.sum
    val time2NextEvent = Sampler.sampleExponential(totalEventRate)
    // figure out the event happens at which node by sample eventRates according to its weights
    if (eventRates.isEmpty) {
      return false
    }
    val eventNode: NodeType = Sampler.samplePMF[NodeType](eventRates)
    eventNode.resolveEvent() // update internal state
    time += time2NextEvent
    true
  }
}
