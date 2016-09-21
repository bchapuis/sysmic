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

package sysmic.distance

import sysmic.geometry.Point

trait Distance {

  /**
    * Computes de distance in meters between two lat/lng points.
    *
    * @param p1
    * @param p2
    * @return
    */
  def distance(p1:Point, p2:Point): Double = {
    distance(p1.x, p1.y, p2.x, p2.y)
  }

  /**
    * Computes de distance in meters between two lat/lng points.
    *
    * @param lon1
    * @param lat1
    * @param lon2
    * @param lat2
    * @return
    */
  def distance(lon1:Double, lat1:Double, lon2:Double, lat2:Double):Double

}
