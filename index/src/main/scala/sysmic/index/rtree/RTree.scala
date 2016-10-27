/*
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sysmic.index.rtree

import sysmic.geometry.{BBox, Point}
import sysmic.geometry.SpatialDataUtil._
import sysmic.index.rtree.RTree._

import scala.runtime.ScalaRunTime

trait Splitter {
  def split(entries: List[Entry]): (List[Entry], List[Entry])
}

object RTree {

  case class Node(children: List[Entry])

  case class Entry(bbox: BBox, value: Any) {
    override lazy val hashCode = ScalaRunTime._hashCode(this)
  }

//  case class BBox(val p1.x: Double, val p1.y: Double, val p2.x: Double, val p2.y: Double) {
//
//    assert(p1.x <= p2.x)
//    assert(p1.y <= p2.y)
//
//    lazy val width = p2.x - p1.x
//    lazy val height = p2.x - p1.x
//
//    lazy val size = width * height
//
//    def contains(bbox: BBox): Boolean = {
//      p1.x <= bbox.p1.x &&
//      p2.x >= bbox.p2.x &&
//      p1.y <= bbox.p1.y &&
//      p2.y >= bbox.p2.y
//    }
//
//    def overlap(bbox: BBox): Boolean = {
//      p1.x <= bbox.p2.x &&
//      p2.x >= bbox.p1.x &&
//      p1.y <= bbox.p2.y &&
//      p2.y >= bbox.p1.y
//    }
//
//  }

  def wrap(entries: List[Entry]): BBox = {
    val minX = entries.map(_.bbox.p1.x).min
    val maxX = entries.map(_.bbox.p2.x).max
    val minY = entries.map(_.bbox.p1.y).min
    val maxY = entries.map(_.bbox.p2.y).max
    BBox(Point(minX, minY), Point(maxX, maxY))
  }

}

class RTree[V](val M: Int, val m: Int, val root: Node, val splitter: Splitter) {
  assert(root != null)
  assert(splitter != null)

  def search(s: BBox): Stream[V] = {
    searchEntries(s).map(_.value.asInstanceOf[V])
  }

  private[rtree] def searchEntries(s: BBox): Stream[Entry] = {
    search(s, root, 0)
  }

  private[rtree] def search(s: BBox, t: Node, d: Int): Stream[Entry] = {
    if (isLeaf(t)) {
      t.children.toStream.filter(e => bboxOverlap(s, e.bbox))
    } else {
      t.children.toStream
        .filter(e => bboxOverlap(s, e.bbox))
        .flatMap(e => search(s, e.value.asInstanceOf[Node], d + 1))
    }
  }

  def insert(key: BBox, value: V): RTree[V] = {
    insert(Entry(key, value))
  }

  private[rtree] def insert(entry: Entry): RTree[V] = {
    insert(entry, root) match {
      case Left(n) =>
        new RTree(M, m, n, splitter)
      case Right((n1, n2)) =>
        val entries = List(Entry(wrap(n1.children), n1), Entry(wrap(n2.children), n2))
        new RTree(M, m, Node(entries), splitter)
    }
  }

  private[rtree] def insert(entry: Entry, node: Node): Either[Node, (Node, Node)] = {
    if (isLeaf(node)) {
      insertLeaf(entry, node)
    } else {
      insertTree(entry, node)
    }
  }

  private[rtree] def insertLeaf(entry: Entry, leaf: Node): Either[Node, (Node, Node)] = {
    if (leaf.children.size < M) {
      Left(Node(leaf.children :+ entry))
    } else {
      val (n1, n2) = splitter.split(leaf.children :+ entry)
      Right((Node(n1), Node(n2)))
    }
  }

  private[rtree] def insertTree(entry: Entry, tree: Node): Either[Node, (Node, Node)] = {
    val l = tree.children.minBy(f => bboxArea(wrap(List(f, entry))) - bboxArea(f.bbox))
    val d = tree.children.diff(List(l))
    insert(entry, l.value.asInstanceOf[Node]) match {
      case Left(n) =>
        Left(Node(Entry(wrap(n.children), n) :: d))
      case Right((n1, n2)) =>
        if (tree.children.size < M) {
          val entries = d ++ List(Entry(wrap(n1.children), n1), Entry(wrap(n2.children), n2))
          Left(Node(entries))
        } else {
          val entries = d ++ List(Entry(wrap(n1.children), n1), Entry(wrap(n2.children), n2))
          val (n11, n22) = splitter.split(entries)
          Right((Node(n11), Node(n22)))
        }
    }
  }

  def delete(key: BBox, value: V): RTree[V] = {
    delete(Entry(key, value))
  }

  private[rtree] def delete(entry: Entry): RTree[V] = {
    val (n, q) = delete(entry, root, List())
    val tree = if (!isLeaf(n) && n.children.size == 1) {
      val child = n.children.head.value.asInstanceOf[Node]
      new RTree[V](M, m, child, splitter)
    } else {
      new RTree[V](M, m, n, splitter)
    }
    q.flatMap(n => leafEntries(n)).foldLeft(tree)(_.insert(_))
  }

  private[rtree] def leafEntries(node: Node): List[Entry] = {
    if (isLeaf(node)) {
      node.children
    } else {
      node.children.flatMap(n => leafEntries(n.value.asInstanceOf[Node]))
    }
  }

  private[rtree] def delete(entry: Entry, node: Node, q: List[Node]): (Node, List[Node]) = {
    if (isLeaf(node)) {
      (deleteLeaf(entry, node), q)
    } else {
      deleteTree(entry, node, q)
    }
  }

  private[rtree] def deleteLeaf(entry: Entry, leaf: Node): Node = {
    val children = leaf.children.filter(e => e != entry)
    Node(children)
  }

  private[rtree] def deleteTree(entry: Entry, tree: Node, q: List[Node]): (Node, List[Node]) = {

    // non-overlapping entries
    val noe = tree.children.filter(child => !bboxOverlap(entry.bbox, child.bbox))

    // overlapping entries
    val oe = tree.children.diff(noe)

    // overlapping nodes after deletion and subsequent nodes to reinsert
    val onsq = oe.map(child => delete(entry, child.value.asInstanceOf[Node], List()))

    // overlapping nodes after deletion
    val on = onsq.map(_._1)

    // subsequent nodes to reinsert
    val sq = onsq.flatMap(_._2)

    // nodes to reinsert
    val nq = on.filter(n => n.children.size < m)

    // remaining entries
    val re = on.diff(nq).map(n => Entry(wrap(n.children), n))

    (Node(noe ++ re), q ++ sq ++ nq)
  }

  private[rtree] def depth(n: Node): Int = {
    if (isLeaf(n)) 1 else 1 + depth(n.children.head.value.asInstanceOf[Node])
  }

  private[rtree] def isLeaf(n: Node): Boolean = {
    (n.children.isEmpty) || !n.children.head.value.isInstanceOf[Node]
  }

}
