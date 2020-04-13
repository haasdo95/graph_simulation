package SIR

import graph.{Node, State}

object SIRCompartments extends Enumeration {
  type SIRCompartments = Value
  val SUSCEPTIBLE, INFECTED, REMOVED = Value
}
import SIR.SIRCompartments._

class SIRState(var compartment: SIRCompartments) extends State

class SIRNode(override val id: Int, override val state: SIRState, infectionRate: Double, recoveryRate: Double) extends Node(id = id, state = state) {
  private var transition: Map[SIRCompartments, Option[Double]] = _
  override def eventRate(neighbors: Set[Node]): Option[Double] = {
    val sirNeighbors = neighbors.asInstanceOf[Set[SIRNode]]
    def gatherInfection(): Option[Double] = {
      val numInfectedNeighbors = sirNeighbors.count(n=>n.state.compartment == INFECTED)
      if (numInfectedNeighbors == 0) None
      else Some(numInfectedNeighbors * infectionRate)
    }

    transition = state.compartment match {
      case SUSCEPTIBLE => Map(INFECTED -> gatherInfection())
      case INFECTED => Map(REMOVED -> Some(recoveryRate))
      case REMOVED => Map()
    }

    var eventful = false
    var nodeTotalRate = 0.0
    for (eventRate <- transition.values) {
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
    assert(transition != null)
    // won't be called if the node isn't eventful at all
    val newStateType = utils.Sampler.samplePMF[SIRCompartments](transition)
    transition = null // now obsolete
    state.compartment = newStateType
  }
}

