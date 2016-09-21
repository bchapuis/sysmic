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

package ch.doplab.spatial.algorithm

import ch.doplab.spatial.geometry.{Geometry, Point}

/**
  * The Ramer–Douglas–Peucker algorithm (RDP) is an algorithm for reducing the number
  * of points in a curve that is approximated by a series of points.
  *
  * Sources:
  * - https://en.wikipedia.org/wiki/Ramer–Douglas–Peucker_algorithm
  */
object RamerDouglasPeucker {

  def run(points:List[Point], epsilon:Double):List[Point] = {
    assert(epsilon > 0)
    if (points.size <= 2) {
      points
    } else {
      val first = points.head
      val last = points.last

      // Find the point with the maximum distance
      val (index, distance) = points.zipWithIndex
        .map(t => (t._2, Point.perpendicularDistance(t._1, first, last)))
        .maxBy(t => t._2)

      // If max distance is greater than epsilon, recursively simplify
      if (distance > epsilon) {
        val r1 = run(points.slice(0, index), epsilon)
        val r2 = run(points.slice(index, points.size - 1), epsilon)
        r1 ++ r2
      } else {
        List(first, last)
      }
    }
  }

}
