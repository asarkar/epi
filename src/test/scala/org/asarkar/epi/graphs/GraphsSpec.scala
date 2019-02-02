package org.asarkar.epi.graphs

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.config.CoreConfig

import scala.reflect.ClassTag

class GraphsSpec extends FlatSpec {
  "graphs" should "detect a cycle if present" in {
    val builder = Graph.newBuilder[Int, DiEdge](implicitly[ClassTag[DiEdge[Int]]], CoreConfig())

    builder += DiEdge(1, 2)
    builder += DiEdge(2, 3)
    builder += DiEdge(1, 3)
    builder += DiEdge(4, 1)
    builder += DiEdge(4, 5)
    builder += DiEdge(5, 6)
    builder += DiEdge(6, 4)

    val g = builder.result()
    isSimple(g) shouldBe false
    isSimple(g.filterNot(_ ==  DiEdge(6, 4))) shouldBe true
  }
}
