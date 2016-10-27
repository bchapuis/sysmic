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

import sysmic.geometry.GeometryUtil._
import sysmic.index.rtree.RTree._

/**
  * Created by bchapuis on 04/04/16.
  */
class LinearSplitter(M: Int, m: Int) extends Splitter {

  /**
    * Divide a set of M+1 index entries into two groups.
    *
    * @param entries
    * @return
    */
  def split(entries: List[Entry]):(List[Entry], List[Entry]) = {
    assert(entries != null)
    assert(entries.size == M + 1)
    val (s1, s2, r) = pickSeeds(entries)
    val (ss1, ss2, rr) = split(s1, s2, r)
    (ss1, ss2)
  }

  private[rtree] def split(g1: List[Entry], g2: List[Entry], entries: List[Entry]): (List[Entry], List[Entry], List[Entry]) = {
    if (entries.isEmpty) {
      (g1, g2, entries)
    } else if (g1.size + entries.size == m) {
      (g1 ++ entries, g2, entries)
    } else if (g2.size + entries.size == m) {
      (g1, g2 ++ entries, entries)
    } else {
      val (entry, remaining) = pickNext(g1, g2, entries)
      val a1 = bboxArea(wrap(g1))
      val a2 = bboxArea(wrap(g2))
      val c1 = bboxArea(wrap(entry :: g1)) - a1
      val c2 = bboxArea(wrap(entry :: g2)) - a2
      if (c1 < c2) {
        split(g1 :+ entry, g2, remaining)
      } else if (c2 < c1) {
        split(g1, g2 :+ entry, remaining)
      } else if (a1 < a2) {
        split(g1 :+ entry, g2, remaining)
      } else if (a2 < a1) {
        split(g1, g2 :+ entry, remaining)
      } else if (g1.size <= g2.size) {
        split(g1 :+ entry, g2, remaining)
      } else {
        split(g1, g2 :+ entry, remaining)
      }
    }
  }

  private[rtree] def pickNext(g1: List[Entry], g2: List[Entry], entries: List[Entry]): (Entry, List[Entry]) = {
    // Choose any of the remaining entries
    (entries.head, entries.tail)
  }

  private[rtree] def pickSeeds(entries: List[Entry]): (List[Entry], List[Entry], List[Entry]) = {
    // BBox used for separation normalization.
    val r = wrap(entries)

    // Along each dimensions,
    // find the entry whose rectangle has the highest low side,
    // and the one with the lowest hight side.

    val hlsx = entries.maxBy(e => e.bbox.p1.x)
    val lhsx = entries.minBy(e => e.bbox.p2.x)
    val sepx = (hlsx.bbox.p2.x - lhsx.bbox.p1.x) / (r.p2.x - r.p1.x)

    val hlsy = entries.maxBy(e => e.bbox.p1.y)
    val lhsy = entries.minBy(e => e.bbox.p2.y)
    val sepy = (hlsy.bbox.p2.y - lhsy.bbox.p1.y) / (r.p2.y - r.p1.y)

    // Select the most extreme pair
    val maxSeparation = List(
        (hlsx, lhsx, sepx),
        (hlsy, lhsy, sepy)
    ).maxBy(_._3)

    (List(maxSeparation._1), List(maxSeparation._2), entries.diff(List(maxSeparation._1, maxSeparation._2)))
  }

}
