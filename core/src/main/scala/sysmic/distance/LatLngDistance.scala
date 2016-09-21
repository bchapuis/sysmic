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

import scala.math._

object LatLngDistance extends Distance {

  val deglen = 110.25

  val R = 6371e3;

  def distance(lon1:Double, lat1:Double, lon2:Double, lat2:Double):Double = {
    val φ1 = lat1.toRadians
    val φ2 = lat2.toRadians
    val Δφ = (lat2-lat1).toRadians
    val Δλ = (lon2-lon1).toRadians
    val a = sin(Δφ/2) * sin(Δφ/2) +
      cos(φ1) * cos(φ2) *
        sin(Δλ/2) * sin(Δλ/2)
    val c = 2 * atan2(sqrt(a), sqrt(1-a))
    val d = R * c
    d
  }

}


