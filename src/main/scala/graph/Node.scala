package graph

trait State
class NamedState(var name: String) extends State

abstract class Node(val id: Int, val state: State) {
  def eventRate(neighbors: Set[Node]): Option[Double] // aggregate Poisson rate of seeing an event at this node
  def resolveEvent() // per-chance an event occurs at the node, figure out event type and update state accordingly

  override def equals(obj: Any): Boolean =
    obj match {
      case node: Node => node.id == id
      case _ => false
    }
  override def hashCode(): Int = id
}

abstract class NamedNode(override val id: Int, override val state: NamedState) extends Node(id, state)
