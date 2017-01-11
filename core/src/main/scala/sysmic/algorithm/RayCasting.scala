package sysmic.algorithm

import sysmic.geometry.{Point, Polygon}
import scala.math._

/**
  * Source:
  * - https://rosettacode.org/wiki/Ray-casting_algorithm
  */
object RayCasting {

  final val epsilon = 0.00001

  def contains(polygon:Polygon, point:Point):Boolean = {
    val count = polygon.points.sliding(2)
      .count(edge => rayIntersectsSegment(point, edge(0), edge(1)))
    count % 2 == 1
  }

  def rayIntersectsSegment(p:Point, a:Point, b:Point):Boolean = {
    if (p.y ==a.y || p.y == b.y) {
      rayIntersectsSegment(Point(p.x, p.y + epsilon), a, b)
    } else {
      if (p.y < a.y || p.y > b.y) {
        false
      } else if (p.x > max(a.x, b.x)){
        false
      } else if (p.x < min(a.x, b.x)) {
        true
      } else {
        val red = if (a.x != b.x) {
          (b.y - a.y) / (b.x - a.x)
        } else {
          Double.MaxValue
        }
        val blue = if (a.x != p.x) {
          (p.y - a.y) / (p.x - a.x)
        } else {
          Double.MaxValue
        }
        blue >= red
      }
    }
  }

}
