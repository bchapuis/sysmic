/*
 * Licensed under the Apache License, Version 2.0 (the "License")
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

package ch.doplab.spatial.geometry

trait Data

/**
  * A base trait for all geometries.
  */
trait Geometry extends Data {

  /**
    * Compute the minimum Bounding Box of this geometry.
    *
    * @return
    */
  def bbox():BBox

}


/**
  * Utility functions for geometries.
  */
object Geometry {

  def contains(r:BBox, p:Point):Boolean = {
    r.p1.x <= p.x &&
      r.p1.y <= p.y &&
      r.p2.x >= p.x &&
      r.p2.y >= p.y
  }

  def contains(r:BBox, s:BBox):Boolean = {
    r.p1.x <= s.p1.x &&
      r.p2.x >= s.p2.x &&
      r.p1.y <= s.p1.y &&
      r.p2.y >= s.p2.y
  }

  def overlap(r:BBox, s:BBox):Boolean = {
    r.p1.x <= s.p2.x &&
      r.p2.x >= s.p1.x &&
      r.p1.y <= s.p2.y &&
      r.p2.y >= s.p1.y
  }

  def mean(points:List[Point]):Point = {
    Point(points.map(_.x).sum / points.length, points.map(_.y).sum / points.length)
  }

}

/**
  * A two dimensional point.
  *
  * @param x
  * @param y
  */
case class Point(x:Double, y:Double) extends Geometry {

  lazy val bbox = BBox(this, this)

  def cross(p:Point):Double = {
    x * p.y - y * p.x
  }

  def norm():Double = {
    x * x + y * y
  }

  def subtract(p:Point):Point = {
    Point(x - p.x, y - p.y)
  }

  def r():Double = {
    Math.sqrt(x * x + y * y)
  }

  def distance(p:Point):Double = {
    Math.hypot(x - p.x, y - p.y)
  }

  def theta():Double = {
    Math.atan2(y, x)
  }

  def angleTo(p:Point):Double = {
    val dx = p.x - this.x
    val dy = p.y - this.y
    Math.atan2(dy, dx)
  }

  def distanceTo(p:Point):Double = {
    val dx = x - p.x
    val dy = y - p.y
    Math.sqrt(dx * dx + dy * dy)
  }

  def distanceSquaredTo(p:Point):Double = {
    val dx = x - p.x
    val dy = y - p.y
    dx * dx + dy * dy
  }

}

object Point {

  def perpendicularDistance(p: Point, q:Point, r:Point): Double = {
    Math.abs((r.y - q.y) * p.x - (r.x - q.x) * p.y + r.x * q.y - r.y * r.y * q.x) /
      Math.sqrt(Math.pow(r.y - q.y, 2) + Math.pow(r.x - q.x, 2))
  }

  def ccw(a:Point, b:Point, c:Point):Int = {
    val area2 = (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)
    if (area2 < 0) {
      -1
    } else if (area2 > 0) {
      +1
    } else {
      0
    }
  }

  val defaultOrdering = new Ordering[Point] {
    def compare(a: Point, b: Point): Int = {
      if (a.y < b.y) {
        -1
      } else if (a.y > b.y) {
        +1
      } else if (a.x < b.x) {
        -1
      } else if (a.x > b.x) {
        +1
      } else {
        0
      }
    }
  }

  def atan2Ordering(o:Point):Ordering[Point] = new Ordering[Point] {
    def compare(q1: Point, q2: Point): Int = {
      val angle1 = o.angleTo(q1)
      val angle2 = o.angleTo(q2)
      if (angle1 < angle2) {
        -1
      } else if (angle1 > angle2) {
        +1
      } else {
        0
      }
    }
  }

  def polarOrdering(p: Point):Ordering[Point] = new Ordering[Point] {
    def compare(q1: Point, q2: Point): Int = {
      val dx1 = q1.x - p.x
      val dy1 = q1.y - p.y
      val dx2 = q2.x - p.x
      val dy2 = q2.y - p.y
      if (dy1 >= 0 && dy2 < 0) { // q1 above q2 below
        -1
      } else if (dy2 >= 0 && dy1 < 0) { // q1 below q2 above
        +1
      } else if (dy1 == 0 && dy2 == 0) { // 3-collinear and horizontal
        if (dx1 >= 0 && dx2 < 0) {
          -1
        } else if (dx2 >= 0 && dx1 < 0) {
          +1
        } else {
          0
        }
      } else { // both above or below
        -ccw(p, q1, q2)
      }
    }
  }

  def distanceToOrder(o:Point):Ordering[Point] = new Ordering[Point] {
    def compare(p:Point, q:Point):Int = {
      val dist1 = o.distanceSquaredTo(p)
      val dist2 = o.distanceSquaredTo(q)
      if (dist1 < dist2) {
        -1
      } else if (dist1 > dist2) {
        +1
      } else {
        0
      }
    }
  }

}

/**
  * A list of two dimensional points.
  *
  * @param points
  */
case class MultiPoint(points:List[Point]) extends Geometry {

  lazy val bbox = BBox.fromPoints(points)

  lazy val area = 0d

}

/**
  * A two dimensional line.
  *
  * @param points
  */
case class LineString(points:List[Point]) extends Geometry {

  lazy val bbox = BBox.fromPoints(points)

  lazy val area = 0d

}

/**
  * A list of two dimensional lines.
  *
  * @param lines
  */
case class MultiLineString(lines:List[LineString]) extends Geometry {

  lazy val bbox = BBox.fromGeometries(lines)

  lazy val area = 0d

}

/**
  * A two dimensional polygon.
  *
  * @param points
  */
case class Polygon(points:List[Point]) extends Geometry {

  lazy val bbox = BBox.fromPoints(points)

  lazy val area = {
    val n = points.size
    var i = 1
    var sum = 0.0
    for (i <- 1 to n) {
      sum += points(i).x * points(i + 1).y + points(i).y * points(i + 1).x
    }
    sum += points(n-1).x * points(0).y + points(n - 1).y * points(0).x
    sum / 2.0
  }


  def contains(p:Point):Boolean = {
    // http://stackoverflow.com/questions/217578/how-can-i-determine-whether-a-2d-point-is-within-a-polygon
    // https://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html#Explanation
    var c = false
    for (i <- 0 until points.size; j = (i - 1 + points.size) % points.size) {
      if (((points(i).y > p.y) != (points(j).y > p.y))
        && (p.x < (points(j).x - points(i).x) * (p.y - points(i).y) / (points(j).y - points(i).y) + points(i).x)) {
        c = !c
      }
    }
    c
  }

}

/**
  * A list of two dimensional polygons.
  *
  * @param polygons
  */
case class MultiPolygon(polygons:List[Polygon]) extends Geometry {

  lazy val bbox = BBox.fromGeometries(polygons)

  lazy val area = polygons.map(_.area).sum

}

/**
  * A collection of geometries.
  *
  * @param geometries
  */
case class GeometryCollection(geometries:List[Geometry]) extends Geometry {

  lazy val bbox = BBox.fromGeometries(geometries)

}

/**
  * A geometry with properties
  *
  * @param geometry
  * @param id
  * @param properties
  */
case class Feature(geometry: Geometry, properties:Map[String, Any] = Map.empty, id:Option[String] = None) extends Data {
  def bbox(): BBox = geometry.bbox()
}

/**
  * A collection of geometries with properties
  *
  * @param features
  */
case class FeatureCollection(features:List[Feature]) extends Data


/**
  * Utility functions for computing minimum bounding boxes.
  */
object BBox {

  /**
    * Compute the bounding box of a list of points.
    *
    * @param points
    * @return
    */
  def fromPoints(points:List[Point]):BBox = {
    val minX = points.map(_.x).min
    val maxX = points.map(_.x).max
    val minY = points.map(_.y).min
    val maxY = points.map(_.y).max
    BBox(Point(minX, minY), Point(maxX, maxY))
  }

  /**
    * Computes the bounding box of a list of geometries.
    *
    * @param geometries
    * @return
    */
  def fromGeometries(geometries:List[Geometry]):BBox = {
    BBox.fromPoints(geometries.flatMap(g => List(g.bbox.p1, g.bbox.p2)))
  }

}

/**
  * A class for representing bounding boxes.
  *
  * @param p1
  * @param p2
  */
case class BBox(p1:Point, p2:Point) {

  require(p1.x <= p2.x, s"${p1.x} must be smaller or equal to ${p2.x}")
  require(p1.y <= p2.y, s"${p1.y} must be smaller or equal to ${p2.y}")

  lazy val bbox = this

  lazy val area = width * height

  lazy val width = Math.abs(p2.x - p1.x)

  lazy val height = Math.abs(p2.y - p1.y)

}

/**
  * A class for representing circles.
  *
  * @param center
  * @param radius
  */
case class Circle(center:Point, radius:Double) extends Geometry {

  private val ε = 1e-12

  lazy val area = Math.PI * Math.pow(radius, 2)

  def bbox(): BBox = ???

  def contains(p:Point):Boolean = {
    center.distance(p) <= radius + ε
  }

  def contains(points:List[Point]):Boolean = {
    points.takeWhile(point => contains(point)).size == points.size
  }

}