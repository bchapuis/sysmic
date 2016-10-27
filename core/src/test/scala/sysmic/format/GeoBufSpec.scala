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

package sysmic.format

import sysmic.geometry._
import org.scalatest.FunSpec

class GeoBufSpec extends FunSpec {

  import GeoBuf._

  val test = ""
  val point = Point(1.1, 1.1)
  val line = LineString(List(Point(1.1,1.1), Point(2.2,2.2)))
  val polygon = Polygon(List(Point(1.1,1.1), Point(2.2,2.2)))
  val multiPoint = MultiPoint(List(Point(1.1,1.1), Point(2.2,2.2)))
  val multiLineString = MultiLineString(List(LineString(List(Point(1.1,1.1), Point(2.2,2.2))), LineString(List(Point(1.1,1.1), Point(2.2,2.2)))))
  val multiPolygon = MultiPolygon(List(Polygon(List(Point(1.1,1.1), Point(2.2,2.2))), Polygon(List(Point(1.1,1.1), Point(2.2,2.2)))))
  val feature = Feature(Some("id"), point, Map("a" -> "a", "b" -> "b"))
  val featureCollection = FeatureCollection(List(feature, feature))

  describe("GeoBuf Format") {

    it("Point") {
      assert(point == decode(encode(point)))
    }

    it("LineString") {
      assert(line == decode(encode(line)))
    }

    it("Polygon") {
      assert(polygon == decode(encode(polygon)))
    }

    it("MultiPoint") {
      assert(multiPoint == decode(encode(multiPoint)))
    }

    it("MultiLineString") {
      assert(multiLineString == decode(encode(multiLineString)))
    }

    it("MultiPolygon") {
      assert(multiPolygon == decode(encode(multiPolygon)))
    }

    it("Feature") {
      assert(feature == decode(encode(feature)))
    }

    it("FeatureCollection") {
      assert(featureCollection == decode(encode(featureCollection)))
    }

  }

}
