package sysmic.geometry

object EuclideanGeometryUtil {

  def cross(a:Point, b:Point):Double = {
    a.x * b.y - a.y * b.x
  }

  def norm(p:Point):Double = {
    p.x * p.x + p.y * p.y
  }

  def add(a:Point, b:Point):Point = {
    Point(a.x + b.x, a.y + b.y)
  }

  def subtract(a:Point, b:Point):Point = {
    Point(a.x - b.x, a.y - b.y)
  }

  def r(p:Point):Double = {
    Math.sqrt(p.x * p.x + p.y * p.y)
  }

  def distance(a:Point, b:Point):Double = {
    Math.hypot(a.x - b.x, a.y - b.y)
  }

  def theta(p:Point):Double = {
    Math.atan2(p.y, p.x)
  }

  def angleTo(a:Point, b:Point):Double = {
    val dx = b.x - a.x
    val dy = b.y - a.y
    Math.atan2(dy, dx)
  }

  def distanceTo(a:Point, b:Point):Double = {
    Math.sqrt(distanceSquaredTo(a, b))
  }

  def distanceSquaredTo(a:Point, b:Point):Double = {
    val dx = a.x - b.x
    val dy = a.y - b.y
    dx * dx + dy * dy
  }

  def perpendicularDistance(p: Point, q:Point, r:Point): Double = {
    Math.abs((r.y - q.y) * p.x - (r.x - q.x) * p.y + r.x * q.y - r.y * r.y * q.x) /
      Math.sqrt(Math.pow(r.y - q.y, 2) + Math.pow(r.x - q.x, 2))
  }

  def cw(a:Point, b:Point, c:Point):Int = {
    val area2 = (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)
    if (area2 < 0) {
      +1
    } else if (area2 > 0) {
      -1
    } else {
      0
    }
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
      val angle1 = angleTo(o, q1)
      val angle2 = angleTo(o, q2)
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
      if (dy1 >= 0 && dy2 < 0) {
        // q1 above q2 below
        -1
      } else if (dy2 >= 0 && dy1 < 0) {
        // q1 below q2 above
        +1
      } else if (dy1 == 0 && dy2 == 0) {
        // 3-collinear and horizontal
        if (dx1 >= 0 && dx2 < 0) {
          -1
        } else if (dx2 >= 0 && dx1 < 0) {
          +1
        } else {
          0
        }
      } else {
        // both above or below
        cw(p, q1, q2)
      }
    }
  }

  def distanceToOrder(o:Point):Ordering[Point] = new Ordering[Point] {
    def compare(p:Point, q:Point):Int = {
      val dist1 = distanceSquaredTo(o, p)
      val dist2 = distanceSquaredTo(o, q)
      if (dist1 < dist2) {
        -1
      } else if (dist1 > dist2) {
        +1
      } else {
        0
      }
    }
  }

  implicit class EuclideanPoint(a:Point) {

    def +(b:Point) = EuclideanGeometryUtil.add(a, b)

    def -(b:Point) = EuclideanGeometryUtil.subtract(a, b)

  }

}


