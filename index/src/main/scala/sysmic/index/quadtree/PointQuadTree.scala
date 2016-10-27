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

package sysmic.index.quadtree

import sysmic.geometry.{BBox, Point}

/**
  * Created by bchapuis on 07/04/16.
  */
class PointQuadTree[V](val capacity: Int, val root: QuadTree.Node[Point, V]) extends QuadTree[Point, V] {

  import QuadTree._

  def search(s: BBox): Stream[V] = {
    searchEntries(s).map(_.value)
  }

  def searchEntries(s: BBox): Stream[Entry[Point, V]] = {
    search(Some(root), s)
  }

  def search(node: Option[Node[Point, V]], s: BBox): Stream[Entry[Point, V]] = {
    if (node.isDefined || !QuadTree.overlap(s, node.get.boundary)) {
      Stream.empty
    } else {
      val n = node.get
      n.entries.filter(e => QuadTree.contains(s, e.key)).toStream ++
        search(n.nw, s) ++
        search(n.ne, s) ++
        search(n.sw, s) ++
        search(n.se, s)
    }
  }

  def insert(key: Point, value: V): PointQuadTree[V] = {
    val r = insert(root, key, value)
    new PointQuadTree[V](capacity, r)
  }

  def insert(node: Node[Point, V], key: Point, value: V): Node[Point, V] = {
    if (!QuadTree.contains(node.boundary, key)) {
      node
    } else {
      val entries = Entry[Point, V](key, value) :: node.entries
      if (node.entries.size <= capacity && node.nw.isEmpty) {
        Node[Point, V](node.boundary, entries)
      } else if (node.nw.isEmpty) {
        val x = node.boundary.p1.x
        val y = node.boundary.p1.y
        val w = node.boundary.width / 2
        val h = node.boundary.height / 2
        val nw = new BBox(Point(x, y + h), Point(x + w, y + 2 * h))
        val ne = new BBox(Point(x + w, y + h), Point(x + 2 * w, y + 2 * h))
        val sw = new BBox(Point(x, y), Point(x + w, y + h))
        val se = new BBox(Point(x + w, y), Point(x + 2 * w, y + h))
        Node[Point, V]( node.boundary, List(),
            Some(Node(nw, entries.filter(e => QuadTree.contains(nw, e.key)))),
            Some(Node(ne, entries.filter(e => QuadTree.contains(ne, e.key)))),
            Some(Node(sw, entries.filter(e => QuadTree.contains(sw, e.key)))),
            Some(Node(se, entries.filter(e => QuadTree.contains(se, e.key)))))
      } else {
        val nw = insert(node.nw.get, key, value)
        val ne = insert(node.ne.get, key, value)
        val sw = insert(node.sw.get, key, value)
        val se = insert(node.se.get, key, value)
        Node[Point, V](node.boundary, List(), Some(nw), Some(ne), Some(sw), Some(se))
      }
    }
  }

  def delete(key: Point, value: V): PointQuadTree[V] = {
    var r = delete(root, key, value)
    new PointQuadTree[V](capacity, r)
  }

  def delete(node: Node[Point, V], key: Point, value: V): Node[Point, V] = {
    if (node.nw.isEmpty) {
      val entries = node.entries.diff(List(value))
      Node[Point, V](node.boundary, entries)
    } else {
      val nw = delete(node.nw.get, key, value)
      val ne = delete(node.ne.get, key, value)
      val sw = delete(node.sw.get, key, value)
      val se = delete(node.se.get, key, value)
      if (nw.entries.size + ne.entries.size + sw.entries.size + se.entries.size <= capacity) {
        Node[Point, V](node.boundary, nw.entries ++ ne.entries ++ sw.entries ++ se.entries)
      } else {
        Node[Point, V](node.boundary, List(), Some(nw), Some(ne), Some(sw), Some(se))
      }
    }
  }

}
