package org.asarkar.epi

import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge

package object graphs {
  /*
   * 18.4 Deadlock detection
   *
   * ANSWER: We run DFS on the "wait-for" graph. If at anytime during the DFS, we encounter a vertex that's in the
   * grey set, that means it's an predecessor of the current vertex (the edge is a back edge), and there is a cycle.
   */
  def isSimple(g: Graph[Int, DiEdge]): Boolean = {

    val whites = collection.mutable.Set[Int](g.nodes.toOuter.toSeq: _*)
    val blacks = collection.mutable.Set.empty[Int]
    val greys = collection.mutable.Set.empty[Int]

    def visit(v: Int): Boolean = {
      if (greys.contains(v)) false
      else {
        whites.remove(v)
        greys.add(v)

        val simple = g.get(v)
          .outNeighbors
          .iterator
          .map(_.toOuter)
          .filterNot(blacks.contains)
          .forall(visit)

        if (simple) {
          greys.remove(v)
          blacks.add(v)
        }

        simple
      }
    }

    whites
      .iterator
      .forall(visit)
  }
}
