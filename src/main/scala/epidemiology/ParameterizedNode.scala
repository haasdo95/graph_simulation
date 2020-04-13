package epidemiology

import graph.{NamedNode, NamedState, Node}

import scala.collection.Map

class ParameterizedNode[StateType<:NamedState](override val id: Int,
                                               override val state: StateType,
                                               model: CompartmentModel[ParameterizedNode[StateType]])
  extends NamedNode(id, state) {
  private var transitionTable: Map[String, Option[Double]] = _

  override def eventRate(neighbors: Set[Node]): Option[Double] = {
    val ns = neighbors.asInstanceOf[Set[ParameterizedNode[StateType]]]
    transitionTable = model.transition(state.name).map({ case (next, rateFunc) => next -> rateFunc(ns)})

    // sum up event rate, as usual
    var eventful = false
    var nodeTotalRate = 0.0
    for (eventRate <- transitionTable.values) {
      eventRate match {
        case Some(rate) =>
          eventful = true
          nodeTotalRate += rate
        case None =>
      }
    }
    if (eventful) Some(nodeTotalRate)
    else None
  }

  override def resolveEvent(): Unit = {
    assert(transitionTable != null)
    val newStateName = utils.Sampler.samplePMF[String](transitionTable)
    transitionTable = null
    state.name = newStateName
  }
}

