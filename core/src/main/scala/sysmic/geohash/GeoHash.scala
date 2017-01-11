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

package sysmic.geohash

/**
  * Created by bchapuis on 04/04/16.
  *
  * Source: https://github.com/Factual/beercode-open
  */
case class GeoHash(hash:Long, bits:Int, lat:Double, lng:Double, latError:Double, lngError:Double) {

  def base32 = GeoHash.toBase32(hash, bits)

  def minLat:Double = lat - latError
  def maxLat:Double = lat + latError
  def minLng:Double = lng - lngError
  def maxLng:Double = lng + lngError

  def north:GeoHash = GeoHash.fromLong(GeoHash.north(hash))
  def east: GeoHash  = GeoHash.fromLong(GeoHash.east(hash))
  def south:GeoHash = GeoHash.fromLong(GeoHash.south(hash))
  def west: GeoHash  = GeoHash.fromLong(GeoHash.west(hash))

}

object GeoHash {

  val Base32 = Array(
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'b', 'c', 'd', 'e', 'f', 'g',
    'h', 'j', 'k', 'm', 'n', 'p', 'q', 'r',
    's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
  )

  val Base32inv = Array.ofDim[Byte]('z' + 1)
  for (i <- 0 until Base32.length) {
    Base32inv(Base32(i)) = i.toByte
  }

  def encode(lat:Double, lng:Double, bits:Int):GeoHash = {
    val hash = fromLatLng(lat,lng, bits)
    val base32 = toBase32(hash, bits)
    geoHash(base32, hash, bits)
  }

  def decode(base32:String):GeoHash = {
    val bits = base32.length * 5
    val hash = fromBase32(base32, bits)
    geoHash(base32, hash, bits)
  }

  def precision(hash:Long):Int = {
    var g = hash
    if (!isTagged(hash)) {
      throw new IllegalArgumentException("Cannot calculate precision of an untagged geohash")
    }
    g &= 0x3fffffffffffffffl
    var bits = 0
    var b = 32
    while (b != 0) {
      if ((g & ~(-1l << (bits | b))) != g) bits |= b
      b >>>= 1
    }
    bits
  }

  def precision(base32:String):Int = {
    base32.length * 5
  }

  def geoHash(base32:String, hash:Long, bits:Int):GeoHash = {
    val shifted = hash << 61 - bits
    val lat = (unwiden(shifted >> 1) & 0x3fffffffl).toDouble / 0x40000000l * 180 - 90
    val lng = (unwiden(shifted) & 0x7fffffffl).toDouble / 0x80000000l * 360 - 180
    val e = error(bits)
    val latError = e * 90
    val lngError = e * (if ((bits & 1) != 0) 90 else 180)
    GeoHash(hash, bits, lat + latError, lng + lngError, latError, lngError)
  }

  def toBase32(hash:Long, bits:Int):String = {
    val chars = Array.ofDim[Char](bits / 5)
    var h = hash
    for (i <- chars.length - 1 to 0 by -1) {
      chars(i) = Base32((h & 0x1fl).toInt)
      h >>= 5
    }
    chars.mkString
  }

  def fromLatLng(lat:Double, lng:Double, bits:Int):Long = {
    val lats = widen(((lat +  90) * 0x80000000l / 180.0).toLong & 0x7fffffffl)
    val lngs = widen(((lng + 180) * 0x80000000l / 360.0).toLong & 0x7fffffffl)
    (lats >> 1 | lngs) >> 61 - bits | precisionTag(bits)
  }

  def fromBase32(base32:String, bits:Int):Long = {
    base32.map(c => Base32inv(c)).foldLeft(0l)((a, b) => (a << 5) | b) | precisionTag(bits)
  }

  def fromLong(hash:Long):GeoHash = {
    val bits = precision(hash)
    val base32 = toBase32(hash, bits)
    geoHash(base32, hash, bits)
  }

  def isTagged(hash:Long):Boolean = {
    (hash & 0x4000000000000000l) != 0
  }

  def unTag(hash:Long):Long = {
    if (isTagged(hash)) hash & ~precisionTag(precision(hash)) else hash
  }

  def precisionTag(bits: Int):Long = {
    0x4000000000000000l | 1l << bits
  }

  def shift(hash:Long, bits:Int, dx:Long, dy:Long):Long = {
    val swap = (bits & 1) == 0
    val sx = if(swap) dy else dx
    val sy = if(swap) dx else dy
    (widen(unwiden(hash >> 1) + sy) << 1 | widen(unwiden(hash) + sx)) & ~(-1l << bits) | precisionTag(bits)
  }

  def shift(taggedHash:Long, dx:Long, dy:Long):Long = shift(taggedHash, precision(taggedHash), dx, dy)

  def north(hash:Long, bits:Int):Long = shift(hash, bits, 0, 1)
  def east (hash:Long, bits:Int):Long = shift(hash, bits, 1, 0)
  def south(hash:Long, bits:Int):Long = shift(hash, bits, 0, -1)
  def west (hash:Long, bits:Int):Long = shift(hash, bits, -1, 0)

  def north(taggedHash:Long):Long = north(taggedHash, precision(taggedHash))
  def east (taggedHash:Long):Long = east (taggedHash, precision(taggedHash))
  def south(taggedHash:Long):Long = north(taggedHash, precision(taggedHash))
  def west (taggedHash:Long):Long = west (taggedHash, precision(taggedHash))

  def unionPrecision(hash1:Long, hash2:Long):Int = {
    val d = hash1 ^ hash2
    if (d == 0) 0 else {
      var bits = 0
      var b = 32
      while (b != 0) {
        if ((d & ~(-1l << (bits | b))) != d) bits |= b
        b >>>= 1
      }
      bits
    }
  }

  def widen(i:Long):Long = {
    var x = i
    x |= x << 16; x &= 0x0000ffff0000ffffl
    x |= x << 8;  x &= 0x00ff00ff00ff00ffl
    x |= x << 4;  x &= 0x0f0f0f0f0f0f0f0fl
    x |= x << 2;  x &= 0x3333333333333333l
    x |= x << 1;  x &= 0x5555555555555555l
    x
  }

  def unwiden(i:Long):Long = {
    var x = i
    x &= 0x5555555555555555l
    x ^= x >> 1;  x &= 0x3333333333333333l
    x ^= x >> 2;  x &= 0x0f0f0f0f0f0f0f0fl
    x ^= x >> 4;  x &= 0x00ff00ff00ff00ffl
    x ^= x >> 8;  x &= 0x0000ffff0000ffffl
    x ^= x >> 16; x &= 0x00000000ffffffffl
    x
  }

  def error(bits:Int):Double = {
    var error = 1.0
    if ((bits & 32) != 0) error *= 0.25
    if ((bits & 16) != 0) error *= 0.5; error *= error
    if ((bits & 8)  != 0) error *= 0.5; error *= error
    if ((bits & 4)  != 0) error *= 0.5; error *= error
    if ((bits & 2)  != 0) error *= 0.5
    error
  }

}
