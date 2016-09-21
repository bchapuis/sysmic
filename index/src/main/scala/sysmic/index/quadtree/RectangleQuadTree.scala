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

import sysmic.geometry.{Point, BBox}

/**
  * Created by bchapuis on 27/04/16.
  */
class RectangleQuadTree[V](val root: QuadTree.Node[BBox, V],
                           val maxDepth: Int)
    extends QuadTree[BBox, V] {

  import QuadTree._

  def search(s: BBox): Stream[V] = {
    searchEntries(s).map(_.value)
  }

  def searchEntries(s: BBox): Stream[Entry[BBox, V]] = {
    search(Some(root), s)
  }

  def search(node: Option[Node[BBox, V]],
             s: BBox): Stream[Entry[BBox, V]] = {
    if (node.isDefined && overlap(s, node.get.boundary)) {
      node.get.entries.filter(e => QuadTree.overlap(s, e.key)).toStream ++
      search(node.get.nw, s) ++
      search(node.get.ne, s) ++
      search(node.get.sw, s) ++
      search(node.get.se, s)
    } else {
      Stream.empty
    }
  }

  def insert(key: BBox, value: V): RectangleQuadTree[V] = {
    val r = insert(root, key, value, 0)
    new RectangleQuadTree[V](r, maxDepth)
  }

  private def insert(node: Node[BBox, V],
                     key: BBox,
                     value: V,
                     depth: Int): Node[BBox, V] = {
    val x1 = node.boundary.p1.x
    val x2 = node.boundary.p2.x
    val x3 = x1 + (x2 - x1) / 2
    val y1 = node.boundary.p1.y
    val y4 = node.boundary.p2.y
    val y3 = y1 + (y4 - y1) / 2
    if (depth >= maxDepth) {
      Node[BBox, V](node.boundary,
                         Entry[BBox, V](key, value) :: node.entries,
                         node.nw,
                         node.ne,
                         node.sw,
                         node.se)
    } else if (key.p1.x <= x3 && key.p2.y <= y3) {
      val sw = node.sw.getOrElse(Node[BBox, V](BBox(Point(x1, y1), Point(x3, y3))))
      Node[BBox, V](node.boundary,
                         node.entries,
                         node.nw,
                         node.ne,
                         Some(insert(sw, key, value, depth + 1)),
                         node.se)
    } else if (key.p1.x >= x3 && key.p2.y <= y3) {
      val se = node.se.getOrElse(Node[BBox, V](BBox(Point(x3, y1), Point(x2, y3))))
      Node[BBox, V](node.boundary,
                         node.entries,
                         node.nw,
                         node.ne,
                         node.sw,
                         Some(insert(se, key, value, depth + 1)))
    } else if (key.p1.x >= x3 && key.p1.y >= y3) {
      val ne = node.ne.getOrElse(Node[BBox, V](BBox(Point(x3, y3), Point(x2, y4))))
      Node[BBox, V](node.boundary,
                         node.entries,
                         node.nw,
                         Some(insert(ne, key, value, depth + 1)),
                         node.sw,
                         node.se)
    } else if (key.p2.x <= x3 && key.p1.y >= y3) {
      val nw = node.nw.getOrElse(Node[BBox, V](BBox(Point(x1, y3), Point(x3, y4))))
      Node[BBox, V](node.boundary,
                         node.entries,
                         Some(insert(nw, key, value, depth + 1)),
                         node.ne,
                         node.sw,
                         node.se)
    } else {
      Node[BBox, V](node.boundary,
                         Entry[BBox, V](key, value) :: node.entries,
                         node.nw,
                         node.ne,
                         node.sw,
                         node.se)
    }
  }

  def delete(key: BBox, value: V): RectangleQuadTree[V] = {
    var r = delete(root, key, value)
    new RectangleQuadTree[V](r, maxDepth)
  }

  private def delete(node: Node[BBox, V],
                     key: BBox,
                     value: V): Node[BBox, V] = {
    val x1 = node.boundary.p1.x
    val x2 = node.boundary.p2.x
    val x3 = x1 + (x2 - x1) / 2
    val y1 = node.boundary.p1.y
    val y4 = node.boundary.p2.y
    val y3 = y1 + (y4 - y1) / 2
    if (key.p1.x <= x3 && key.p2.y <= y3 && node.sw.isDefined) {
      Node[BBox, V](node.boundary,
                         node.entries,
                         node.nw,
                         node.ne,
                         Some(delete(node.sw.get, key, value)),
                         node.se)
    } else if (key.p1.x >= x3 && key.p2.y <= y3 && node.sw.isDefined) {
      Node[BBox, V](node.boundary,
                         node.entries,
                         node.nw,
                         node.ne,
                         node.sw,
                         Some(delete(node.se.get, key, value)))
    } else if (key.p1.x >= x3 && key.p1.y >= y3 && node.sw.isDefined) {
      Node[BBox, V](node.boundary,
                         node.entries,
                         node.nw,
                         Some(delete(node.ne.get, key, value)),
                         node.sw,
                         node.se)
    } else if (key.p2.x <= x3 && key.p1.y >= y3 && node.sw.isDefined) {
      Node[BBox, V](node.boundary,
                         node.entries,
                         Some(delete(node.nw.get, key, value)),
                         node.ne,
                         node.sw,
                         node.se)
    } else {
      val entries = node.entries.diff(List(Entry(key, value)))
      Node[BBox, V](node.boundary,
                         entries,
                         node.nw,
                         node.ne,
                         node.sw,
                         node.se)
    }
  }
}
