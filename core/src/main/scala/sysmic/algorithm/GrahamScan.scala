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

package sysmic.algorithm

import java.util

import sysmic.geometry.{EuclideanGeometryUtil, Point}

/**
  * The GrahamScan helper object provides methods for computing the
  * convex hull of a set of points in the plane.
  *
  * Sources:
  * - http://jamesadam.me/index.php/2013/11/21/grahams-scan-in-scala/
  * - http://algs4.cs.princeton.edu/99hull/Point2D.java.html
  * - http://algs4.cs.princeton.edu/99hull/GrahamScan.java.html
  */
object GrahamScan {

  import EuclideanGeometryUtil._

  // Scan the List of coordinates and find our vertices
  private def scan(points: List[Point]): List[Point] = points match {
    case xs if xs.isEmpty => List()
    case xs if xs.size == 2 => points
    case x::y::z::xs if ccw(x,y,z) > 0 => x::scan(y::z::xs)
    case x::y::z::xs => scan(x::z::xs)
  }

  def run(points:List[Point]):List[Point] = {
    // Find the coordinate with the lowest latitude
    val origin = points.minBy(_.y)

    // Sort the rest of the points according to their polar angle
    // (the angle between the line defined by the origin and the current point, and the x-axis)
    val coordList = origin :: points
      .filterNot(_ == origin)
      .sorted(EuclideanGeometryUtil.atan2Ordering(origin))

    // Do the Graham scan
    scan(coordList)
  }

}
