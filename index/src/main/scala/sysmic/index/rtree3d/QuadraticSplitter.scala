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

package sysmic.index.rtree3d

import sysmic.index.rtree3d.RTree3D._

/**
  * Created by bchapuis on 02/04/16.
  */
class QuadraticSplitter(M: Int, m: Int) extends Splitter {

  /**
    * Divide a set of M+1 index entries into two groups.
    *
    * @param entries
    * @return
    */
  def split(entries: List[Entry]): (List[Entry], List[Entry]) = {
    assert(entries != null)
    assert(entries.size == M + 1)
    val (s1, s2, r) = pickSeeds(entries)
    val (ss1, ss2, rr) = split(s1, s2, r)
    (ss1, ss2)
  }

  private[rtree3d] def pickSeeds(
      entries: List[Entry]): (List[Entry], List[Entry], List[Entry]) = {
    val (s1, s2) = entries
      .flatMap(e1 => entries.map(e2 => (e1, e2)))
      .maxBy(p => {
        val j = wrap(List(p._1, p._2))
        j.size - p._1.mbb.size - p._2.mbb.size
      })
    (List(s1), List(s2), entries.diff(List(s1, s2)))
  }

  private[rtree3d] def split(
      g1: List[Entry],
      g2: List[Entry],
      entries: List[Entry]): (List[Entry], List[Entry], List[Entry]) = {
    if (entries.isEmpty) {
      (g1, g2, entries)
    } else if (g1.size + entries.size == m) {
      (g1 ++ entries, g2, entries)
    } else if (g2.size + entries.size == m) {
      (g1, g2 ++ entries, entries)
    } else {
      val (entry, remaining) = pickNext(g1, g2, entries)
      val a1 = wrap(g1).size
      val a2 = wrap(g2).size
      val c1 = wrap(entry :: g1).size - a1
      val c2 = wrap(entry :: g2).size - a2
      if (c1 < c2) {
        split(g1 :+ entry, g2, remaining)
      } else if (c2 < c1) {
        split(g1, entry :: g2, remaining)
      } else if (a1 < a2) {
        split(entry :: g1, g2, remaining)
      } else if (a2 < a1) {
        split(g1, entry :: g2, remaining)
      } else if (g1.size <= g2.size) {
        split(entry :: g1, g2, remaining)
      } else {
        split(g1, entry :: g2, remaining)
      }
    }
  }

  private[rtree3d] def pickNext(g1: List[Entry],
                                g2: List[Entry],
                                entries: List[Entry]): (Entry, List[Entry]) = {
    val entry = entries.maxBy(entry => {
      val d1 = wrap(entry :: g1).size - wrap(g1).size
      val d2 = wrap(entry :: g2).size - wrap(g2).size
      Math.abs(d2 - d1)
    })
    (entry, entries.diff(List(entry)))
  }

}
