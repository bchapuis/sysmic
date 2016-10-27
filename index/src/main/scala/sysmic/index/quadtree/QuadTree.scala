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
import sysmic.index.quadtree.QuadTree.{Entry, Node}

import scala.runtime.ScalaRunTime
import scala.math._

/**
  * Created by bchapuis on 28/04/16.
  */
object QuadTree {

  case class Node[K, V](boundary: BBox, entries: List[Entry[K, V]] = List.empty,
                        nw: Option[Node[K, V]] = None,
                        ne: Option[Node[K, V]] = None,
                        sw: Option[Node[K, V]] = None,
                        se: Option[Node[K, V]] = None)

  case class Entry[K, V](key: K, value: V) {
    override lazy val hashCode = ScalaRunTime._hashCode(this)
  }

  def depth(y: BBox, z: BBox): Double = log(z.area / y.area) / log(4)

  def contains(r: BBox, p: Point): Boolean = {
    r.p1.x <= p.x &&
    r.p1.y <= p.y &&
    r.p2.x >= p.x &&
    r.p2.y >= p.y
  }

  def contains(r1: BBox, r2: BBox): Boolean = {
    r1.p1.x <= r2.p1.x &&
    r1.p2.x >= r2.p2.x &&
    r1.p1.y <= r2.p1.y &&
    r1.p1.y >= r2.p2.y
  }

  def overlap(r1: BBox, r2: BBox): Boolean = {
    r1.p1.x <= r2.p2.x &&
    r1.p2.x >= r2.p1.x &&
    r1.p1.y <= r2.p2.y &&
    r1.p2.y >= r2.p1.y
  }

}

trait QuadTree[K, V] {

  def root: Node[K, V]

  def search(s: BBox): Stream[V]

  def searchEntries(s: BBox): Stream[Entry[K, V]]

}
