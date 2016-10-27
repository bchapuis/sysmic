package sysmic.geometry

import scala.math._

/**
  * Sources:
  * - http://janmatuschek.de/LatitudeLongitudeBoundingCoordinates#PolesAnd180thMeridian
  */
class SphericalGeometry(R:Double) {

  val MinLat = -90d
  val MaxLat = 90d
  val MinLon = -180d
  val MaxLon = 180d

  def checkBounds(point: Point):Boolean = {
    point.y >= MinLat || point.y <= MaxLat || point.x >= MinLon || point.x <= MaxLon
  }

  def distance(p1:Point, p2:Point):Double = {
    assert(checkBounds(p1), "p1 is out of bounds.")
    assert(checkBounds(p2), "p2 is out of bounds.")
    val p1Lat = p1.y.toRadians
    val p2Lat = p2.y.toRadians
    val p1Lon = p1.x.toRadians
    val p2Lon = p2.x.toRadians
    acos(sin(p1Lat) * sin(p2Lat) + cos(p1Lat) * cos(p2Lat) * cos(p1Lon - p2Lon)) * R
  }

  def boundingBox(p:Point, radius:Double):BBox = {
    assert(checkBounds(p), "p is out of bounds.")
    assert(radius >= 0, "distance must be greater or equal to zero.")

    val radLat = p.y.toRadians
    val radLon = p.x.toRadians

    // angular distance in radians on a great circle
    val radDist = radius / R

    var minLat = radLat - radDist
    var maxLat = radLat + radDist
    var minLon, maxLon = 0d

    if (minLat > MinLat && maxLat < MaxLat) {
      val deltaLon = asin(sin(radDist) / cos(radLat))
      minLon = radLon - deltaLon
      if (minLon < MinLon) minLon += 2d * Pi
      maxLon = radLon + deltaLon
      if (maxLon > MaxLon) maxLon -= 2d * Pi
    } else {
      // a pole is within the distance
      minLat = max(minLat, MinLat.toRadians)
      maxLat = min(maxLat, MaxLat.toRadians)
      minLon = MinLon
      maxLon = MaxLon
    }

    BBox(
      Point(minLon.toDegrees, minLat.toDegrees),
      Point(maxLon.toDegrees, maxLat.toDegrees)
    )
  }

}

object SunGeometry extends SphericalGeometry(695700e3)

object EarthGeometry extends SphericalGeometry(6371e3)

object MoonGeometry extends SphericalGeometry(1737e3)

object JupiterGeometry extends SphericalGeometry(69911e3)

object MercuryGeometry extends SphericalGeometry(2440e3)

object MarsGeometry extends SphericalGeometry(3390e3)

object PlutoGeometry extends SphericalGeometry(1187e3)

object SaturnGeometry extends SphericalGeometry(58232e3)

object TitanGeometry extends SphericalGeometry(2576e3)

object VenusGeometry extends SphericalGeometry(6052e3)

object NeptuneGeometry extends SphericalGeometry(24622e3)

object UranusGeometry extends SphericalGeometry(25362e3)