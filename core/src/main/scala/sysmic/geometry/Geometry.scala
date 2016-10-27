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

package sysmic.geometry


/**
  * A base trait for spatial data.
  */
trait SpatialData

/**
  * A base trait for geometries.
  */
trait Geometry extends SpatialData


/**
  * A two dimensional point.
  *
  * @param x
  * @param y
  */
case class Point(x:Double, y:Double) extends Geometry


/**
  * A list of two dimensional points.
  *
  * @param points
  */
case class MultiPoint(points:List[Point]) extends Geometry

/**
  * A two dimensional line.
  *
  * @param points
  */
case class LineString(points:List[Point]) extends Geometry

/**
  * A list of two dimensional lines.
  *
  * @param lines
  */
case class MultiLineString(lines:List[LineString]) extends Geometry

/**
  * A two dimensional polygon.
  *
  * @param points
  */
case class Polygon(points:List[Point]) extends Geometry

/**
  * A list of two dimensional polygons.
  *
  * @param polygons
  */
case class MultiPolygon(polygons:List[Polygon]) extends Geometry

/**
  * A collection of geometries.
  *
  * @param geometries
  */
case class GeometryCollection(geometries:List[Geometry]) extends Geometry

/**
  * A geometry with properties
  *
  * @param id
  * @param geometry
  * @param properties
  */
case class Feature(id:Option[String], geometry: Geometry, properties:Map[String, Any]) extends SpatialData

/**
  * A collection of geometries with properties
  *
  * @param features
  */
case class FeatureCollection(features:List[Feature]) extends SpatialData

/**
  * A class for representing bounding boxes.
  *
  * @param p1
  * @param p2
  */
case class BBox(p1:Point, p2:Point) {

  require(p1.x <= p2.x, s"${p1.x} must be smaller or equal to ${p2.x}")
  require(p1.y <= p2.y, s"${p1.y} must be smaller or equal to ${p2.y}")

  lazy val width = Math.abs(p2.x - p1.x)
  lazy val height = Math.abs(p2.y - p1.y)
  lazy val area = width * height

}

