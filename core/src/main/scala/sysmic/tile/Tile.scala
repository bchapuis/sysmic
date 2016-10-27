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

package sysmic.tile

/**
  * Created by bchapuis on 08/04/16.
  *
  * Source: http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Scala
  */

import sysmic.geometry.{BBox, Point}

import scala.math._

object Tile {

  def fromBBox(bbox: BBox, z:Short): IndexedSeq[Tile] = {
    val t1 = fromPoint(Point(bbox.p1.x, bbox.p1.y), z)
    val t2 = fromPoint(Point(bbox.p2.x, bbox.p2.y), z)
    val tiles = for (x <- t1.x to t2.x; y <- t1.y to t2.y by -1) yield {
      Tile(x, y, z)
    }
    tiles
  }

  def fromPoint(p:Point, z:Short):Tile = {
    var x = ((p.x + 180.0) / 360.0 * (1<<z)).toInt
    var y = ((1 - log(tan(toRadians(p.y)) + 1 / cos(toRadians(p.y))) / Pi) / 2.0 * (1 << z)).toInt
    if (x < 0) x = 0
    if (x >= (1 << z)) x = (1 << z)
    if (y < 0) y = 0
    if (y >= (1 << z)) y = (1 << z)
    Tile(x, y, z)
  }

}

case class Tile(x: Int, y: Int, z: Short) {

  def bbox():BBox = {
    val p1 = point()
    val p2 = new Tile(x + 1, y - 1, z).point()
    BBox(p1, p2)
  }

  def point():Point = {
    val lng = x.toDouble / (1<<z) * 360.0 - 180.0
    val lat = toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y.toDouble / (1<<z)))))
    Point(lng, lat)
  }

}

