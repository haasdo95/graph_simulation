package epidemiology

import java.io.FileWriter

import graph.{NamedState, SimGraph}
import scalax.collection.GraphEdge.UnDiEdge

class Simulation[StateType<:NamedState](filename: String,
                                        nodes: Seq[ParameterizedNode[StateType]],
                                        edges: Seq[UnDiEdge[ParameterizedNode[StateType]]],
                                        model: CompartmentModel[ParameterizedNode[StateType]]) {
  val simGraph = new SimGraph[ParameterizedNode[StateType]](nodes, edges)
  val fw = new FileWriter(filename, false)
  private val headers = model.compartments
  fw.write(("TIME" +: headers).mkString(",") + "\n")
  def step(): Boolean = {
    val stateCount = simGraph.getGraph.nodes.toIndexedSeq.map(_.toOuter).groupBy(n => n.state.name)
    val newLine = simGraph.getTime.toString +: headers.map(h =>
      if (stateCount.keySet.contains(h)) stateCount(h).size
      else 0
    )
    fw.write(newLine.mkString(",") + "\n")
    simGraph.next()
  }
}
