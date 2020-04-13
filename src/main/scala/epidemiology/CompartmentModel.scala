package epidemiology

import graph.NamedNode
import org.json4s._
import org.json4s.jackson.JsonMethods._

import scala.collection.{Map, mutable}
import scala.io.Source

// the JSON string is supposed to look like:
//{
//  "SUSCEPTIBLE": {
//    "INFECTED": {
//      "neighbor": 10,
//      "trigger": "INFECTED",
//      "rate": 0.056
//    }
//  },
//  "INFECTED": {
//    "REMOVED": {
//      "neighbor": -1,
//      "trigger": "",
//      "rate": 0.243902439
//    }
//  },
//  "REMOVED": {}
//}

class TransitionInfo(val neighbor: Int, val rate: Double, val trigger: String)

class CompartmentModel[NodeType <: NamedNode](specFile: String) {
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats
  private val jsonDict = parse(Source.fromFile(specFile).getLines().mkString).extract[Map[String, Map[String, TransitionInfo]]]
  val compartments: Seq[String] = jsonDict.keySet.toIndexedSeq

  // fill out transition as well
  private def parseTransition(t: Map[String, Map[String, TransitionInfo]]): Map[String, Map[String, Set[NodeType]=>Option[Double]]] = {
    val m: mutable.Map[String, mutable.Map[String, Set[NodeType]=>Option[Double]]] = mutable.Map()
    for ((start, end) <- t) {
      m(start) = mutable.Map()
      for ((next, info) <- end) {
        m(start)(next) = {neighbors: Set[NodeType] =>
          if (info.neighbor > 0) { // neighbors are indeed needed to determine rate
            val count = neighbors.count(n => n.state.name == info.trigger)
            if (count == 0) None
            else Some(count * info.rate)
          } else { // neighbor info not needed; just return rate
            Some(info.rate)
          }
        }
      }
    }
    m
  }
  val transition: Map[String, Map[String, Set[NodeType]=>Option[Double]]] = parseTransition(jsonDict)
}

