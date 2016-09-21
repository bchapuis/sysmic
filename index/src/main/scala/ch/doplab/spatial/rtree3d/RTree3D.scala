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

package ch.doplab.spatial.rtree3d

import ch.doplab.spatial.rtree3d.RTree3D._

import scala.runtime.ScalaRunTime

trait Splitter {
  def split(entries: List[Entry]): (List[Entry], List[Entry])
}

object RTree3D {

  case class Node(children: List[Entry])

  case class Entry(mbb: MBB, value: Any) {
    override lazy val hashCode = ScalaRunTime._hashCode(this)
  }

  case class MBB(val x1: Double,
                 val y1: Double,
                 val z1: Double,
                 val x2: Double,
                 val y2: Double,
                 val z2: Double) {

    assert(x1 <= x2)
    assert(y1 <= y2)
    assert(z1 <= z2)

    lazy val width = x2 - x1
    lazy val height = x2 - x1
    lazy val depth = z2 - z1

    lazy val size = width * height * depth

    def contains(mbb: MBB): Boolean = {
      x1 <= mbb.x1 &&
      x2 >= mbb.x2 &&
      y1 <= mbb.y1 &&
      y2 >= mbb.y2 &&
      z1 <= mbb.z1 &&
      z2 >= mbb.z2
    }

    def overlap(mbb: MBB): Boolean = {
      x1 <= mbb.x2 &&
      x2 >= mbb.x1 &&
      y1 <= mbb.y2 &&
      y2 >= mbb.y1 &&
      z1 <= mbb.z2 &&
      z2 >= mbb.z1
    }

  }

  def wrap(entries: List[Entry]): MBB = {
    val minX = entries.map(_.mbb.x1).min
    val maxX = entries.map(_.mbb.x2).max
    val minY = entries.map(_.mbb.y1).min
    val maxY = entries.map(_.mbb.y2).max
    val minZ = entries.map(_.mbb.z1).min
    val maxZ = entries.map(_.mbb.z2).max
    MBB(minX, minY, maxX, maxY, minZ, maxZ)
  }

}

class RTree3D[V](val M: Int,
                 val m: Int,
                 val root: Node,
                 val splitter: Splitter) {
  assert(root != null)
  assert(splitter != null)

  import splitter._

  def search(mbb: MBB): Stream[V] = {
    searchEntries(mbb).map(_.value.asInstanceOf[V])
  }

  private[rtree3d] def searchEntries(mbb: MBB): Stream[Entry] = {
    search(mbb, root, 0)
  }

  private[rtree3d] def search(mbb: MBB, t: Node, d: Int): Stream[Entry] = {
    if (isLeaf(t)) {
      t.children.toStream.filter(e => mbb.overlap(e.mbb))
    } else {
      t.children.toStream
        .filter(e => mbb.overlap(e.mbb))
        .flatMap(e => search(mbb, e.value.asInstanceOf[Node], d + 1))
    }
  }

  def insert(mbb: MBB, value: V): RTree3D[V] = {
    insert(Entry(mbb, value))
  }

  private[rtree3d] def insert(entry: Entry): RTree3D[V] = {
    insert(entry, root) match {
      case Left(n) =>
        new RTree3D(M, m, n, splitter)
      case Right((n1, n2)) =>
        new RTree3D(M,
                    m,
                    Node(
                        List(
                            Entry(wrap(n1.children), n1),
                            Entry(wrap(n2.children), n2)
                        )),
                    splitter)
    }
  }

  private[rtree3d] def insert(entry: Entry,
                              node: Node): Either[Node, (Node, Node)] = {
    if (isLeaf(node)) {
      insertLeaf(entry, node)
    } else {
      insertTree(entry, node)
    }
  }

  private[rtree3d] def insertLeaf(entry: Entry,
                                  leaf: Node): Either[Node, (Node, Node)] = {
    if (leaf.children.size < M) {
      Left(Node(leaf.children :+ entry))
    } else {
      val (n1, n2) = splitter.split(leaf.children :+ entry)
      Right((Node(n1), Node(n2)))
    }
  }

  private[rtree3d] def insertTree(entry: Entry,
                                  tree: Node): Either[Node, (Node, Node)] = {
    val l = tree.children.minBy(f => wrap(List(f, entry)).size - f.mbb.size)
    val d = tree.children.diff(List(l))
    insert(entry, l.value.asInstanceOf[Node]) match {
      case Left(n) =>
        Left(Node(Entry(wrap(n.children), n) :: d))
      case Right((n1, n2)) =>
        if (tree.children.size < M) {
          Left(
              Node(
                  d ++ List(
                      Entry(wrap(n1.children), n1),
                      Entry(wrap(n2.children), n2)
                  )))
        } else {
          val (n11, n22) = splitter.split(
              d ++ List(
                  Entry(wrap(n1.children), n1),
                  Entry(wrap(n2.children), n2)
              ))
          Right((Node(n11), Node(n22)))
        }
    }
  }

  def delete(mbb: MBB, value: V): RTree3D[V] = {
    delete(Entry(mbb, value))
  }

  private[rtree3d] def delete(entry: Entry): RTree3D[V] = {
    val (n, q) = delete(entry, root, List())
    val tree = if (!isLeaf(n) && n.children.size == 1) {
      val child = n.children.head.value.asInstanceOf[Node]
      new RTree3D[V](M, m, child, splitter)
    } else {
      new RTree3D[V](M, m, n, splitter)
    }
    q.flatMap(n => leafEntries(n)).foldLeft(tree)(_.insert(_))
  }

  private[rtree3d] def leafEntries(node: Node): List[Entry] = {
    if (isLeaf(node)) {
      node.children
    } else {
      node.children.flatMap(n => leafEntries(n.value.asInstanceOf[Node]))
    }
  }

  private[rtree3d] def delete(entry: Entry,
                              node: Node,
                              q: List[Node]): (Node, List[Node]) = {
    if (isLeaf(node)) {
      (deleteLeaf(entry, node), q)
    } else {
      deleteTree(entry, node, q)
    }
  }

  private[rtree3d] def deleteLeaf(entry: Entry, leaf: Node): Node = {
    val children = leaf.children.filter(e => e != entry)
    Node(children)
  }

  private[rtree3d] def deleteTree(entry: Entry,
                                  tree: Node,
                                  q: List[Node]): (Node, List[Node]) = {
    // non-overlapping entries
    val noe = tree.children.filter(child => !entry.mbb.overlap(child.mbb))

    // overlapping entries
    val oe = tree.children.diff(noe)

    // overlapping nodes after deletion and subsequent nodes to reinsert
    val onsq =
      oe.map(child => delete(entry, child.value.asInstanceOf[Node], List()))

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

  private[rtree3d] def depth(n: Node): Int = {
    if (isLeaf(n)) 1 else 1 + depth(n.children.head.value.asInstanceOf[Node])
  }

  private[rtree3d] def isLeaf(n: Node): Boolean = {
    (n.children.isEmpty) ||
    !n.children.head.value.isInstanceOf[Node]
  }

}
