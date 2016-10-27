package sysmic.geometry

/**
  * Utility functions for geometries.
  */
object SpatialDataUtil {

  def bboxContains(bbox:BBox, p:Point):Boolean = {
    bbox.p1.x <= p.x &&
      bbox.p1.y <= p.y &&
      bbox.p2.x >= p.x &&
      bbox.p2.y >= p.y
  }

  def bboxContains(bbox:BBox, s:BBox):Boolean = {
    bbox.p1.x <= s.p1.x &&
      bbox.p2.x >= s.p2.x &&
      bbox.p1.y <= s.p1.y &&
      bbox.p2.y >= s.p2.y
  }

  def bboxOverlap(bbox:BBox, s:BBox):Boolean = {
    bbox.p1.x <= s.p2.x &&
      bbox.p2.x >= s.p1.x &&
      bbox.p1.y <= s.p2.y &&
      bbox.p2.y >= s.p1.y
  }

  def bboxArea(bbox:BBox):Double = {
    val width = bbox.p2.x - bbox.p1.x
    val height = bbox.p2.y - bbox.p1.y
    width * height
  }

  def pointsMean(points:List[Point]):Point = {
    Point(points.map(_.x).sum / points.length, points.map(_.y).sum / points.length)
  }

  def pointBBox(point:Point):BBox = {
    BBox(point, point)
  }
  
  def pointsBBox(points:List[Point]):BBox = {
    val xMin = points.map(_.x).min
    val xMax = points.map(_.x).max
    val yMin = points.map(_.y).min
    val yMax = points.map(_.y).max
    BBox(Point(xMin, yMin), Point(xMax, yMax))
  }

  def spatialDataBBox(data:List[SpatialData]):BBox = {
    val points = data.map(_.bbox()).flatMap(b => List(b.p1, b.p2))
    pointsBBox(points)
  }

  
  
  def polygonContains(polygon: Polygon, point:Point):Boolean = {
    // http://stackoverflow.com/questions/217578/how-can-i-determine-whether-a-2d-point-is-within-a-polygon
    // https://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html#Explanation
    val points = polygon.points
    var c = false
    for (i <- 0 until points.size; j = (i - 1 + points.size) % points.size) {
      if (((points(i).y > point.y) != (points(j).y > point.y))
        && (point.x < (points(j).x - points(i).x) * (point.y - points(i).y) / (points(j).y - points(i).y) + points(i).x)) {
        c = !c
      }
    }
    c
  }

  def polygonArea(polygon:Polygon):Double = {
    val points = polygon.points
    val n = points.size
    var i = 1
    var sum = 0.0
    for (i <- 1 to n) {
      sum += points(i).x * points(i + 1).y + points(i).y * points(i + 1).x
    }
    sum += points(n-1).x * points(0).y + points(n - 1).y * points(0).x
    sum / 2.0
  }

}
