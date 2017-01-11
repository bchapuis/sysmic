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

package sysmic.geometry

import org.scalatest.FunSpec

class SpatialGeometrySpec extends FunSpec {

  describe("SpatialGeometry") {

    it("must accurately compute distances") {
      val statuteOfLiberty = Point(-74.0444, 40.6892)
      val eiffelTower = Point(2.2945, 48.8583)
      assert(EarthGeometryUtil.distance(statuteOfLiberty, eiffelTower) == 5837413.155831484)
    }

    it("must accurately compute bounding boxes") {
      val d = 1000e3
      val M = Point(-39.998183678082775,80.00209693411685)
      assert(EarthGeometryUtil.boundingBox(M, d) == BBox(Point(-104.20671955633055,71.00888087492955),Point(24.210352200164998,88.99531299330417)))
    }

  }

}
