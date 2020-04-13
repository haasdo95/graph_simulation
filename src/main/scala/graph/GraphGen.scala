package graph

import utils.Sampler
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._

import scala.collection.{mutable, Seq}

object GraphGen {
  def erdosRenyi[NodeType <: Node](nodes: Seq[NodeType], p: Double): Seq[UnDiEdge[NodeType]] = {
    var edges: Seq[UnDiEdge[NodeType]] = Seq()
    for (i <- nodes.indices) {
      for (j <- i+1 until nodes.size) {
        if (Sampler.sampleBernoulli(p)) {
          edges = (nodes(i)~nodes(j)) +: edges
        }
      }
    }
    edges
  }

  def wattsStrogatz[NodeType <: Node](nodes: Seq[NodeType], k: Int, p: Double): Seq[UnDiEdge[NodeType]] = {
    assert(k % 2 == 0)
    for (i <- nodes.indices) { // unshuffled nodes
      assert(nodes(i).id == i)
    }
    var edges: mutable.Seq[UnDiEdge[NodeType]] = mutable.Seq()
    // adjacency list to assure forming simple graph
    val adjacencyList = mutable.Map[NodeType, mutable.Set[NodeType]]()
    def toggleEdge(n1: NodeType, n2: NodeType, on: Boolean): Unit = {
      if (!adjacencyList.keySet.contains(n1)) {
        adjacencyList(n1) = mutable.Set[NodeType]()
      }
      if (!adjacencyList.keySet.contains(n2)) {
        adjacencyList(n2) = mutable.Set[NodeType]()
      }
      if (on) {
        adjacencyList(n1).add(n2)
        adjacencyList(n2).add(n1)
      } else {
        adjacencyList(n1).remove(n2)
        adjacencyList(n2).remove(n1)
      }
    }
    // form k-lattice ring
    for (i<-nodes.indices) { // note that edges are added in a "clockwise" manner
      for (j <- 1 to k/2) {
        val rightIdx = Math.floorMod(i + j, nodes.size)
        val leftIdx = Math.floorMod(i - j, nodes.size) // WHY ON EARTH SHOULD MODULO RETURN STH NEGATIVE......
        val rightNode = nodes(rightIdx)
        val leftNode = nodes(leftIdx)
        edges = (nodes(i) ~ rightNode) +: edges // add right neighbors
        // don't need to add left edges; would be double counting
        toggleEdge(nodes(i), rightNode, on = true)
        toggleEdge(leftNode, nodes(i), on = true)
      }
    }
    // rewiring with probability p
    for (i <- edges.indices) {
      if (utils.Sampler.sampleBernoulli(p)) { // need to rewire
        val edge = edges(i)
        val (first, second) = (edge._1, edge._2)
        toggleEdge(first, second, on = false)
        // ideally, the new edge should be wired to a non-neighbor of first, also not first itself
        // this is why we require node.id == its location in nodes; will be quite painful to sample otherwise
        val excluded = adjacencyList(first).map(_.id) + first.id
        val newNeighbor = nodes(utils.Sampler.sampleDiscreteUniform(0, nodes.size, excluded))
        toggleEdge(first, newNeighbor, on = true)
        edges(i) = first ~ newNeighbor
      }
    }
    edges
  }
}
