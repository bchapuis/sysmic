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

package ch.doplab.spatial.format

import java.lang.reflect.Type

import ch.doplab.spatial.geometry._
import com.google.gson._
import scala.collection.JavaConverters._

class GeoJsonSerializer extends JsonSerializer[Geometry] {

  def coordinates(c:AnyRef):AnyRef = c match {
    case Point(x, y) =>
      Array(x, y)
    case points:List[Point] =>
      points.map(coordinates).toArray
    case points:List[List[Point]] =>
      points.map(coordinates).toArray
    case _ =>
      throw new UnsupportedOperationException
  }

  def serializeGeometry(name:String, points:AnyRef, context:JsonSerializationContext):JsonObject = {
    val json = new JsonObject()
    json.addProperty("type", name)
    val c = coordinates(points)
    json.add("coordinates", context.serialize(c))
    json
  }

  def serializeFeature(feature:Feature, context: JsonSerializationContext):JsonObject = {
    val json = new JsonObject()
    json.addProperty("type", "Feature")
    if (!feature.id.isEmpty) {
      json.addProperty("id", feature.id.get)
    }
    if (!feature.properties.isEmpty) {
      val properties = new JsonObject()
      json.add("properties", context.serialize(feature.properties.asJava))
    }
    json.add("geometry", context.serialize(feature.geometry))
    json
  }

  def serializeFeatures(features: FeatureCollection, context: JsonSerializationContext):JsonObject = {
    val json = new JsonObject()
    json.addProperty("type", "FeatureCollection")
    json.add("features", context.serialize(features.features.toArray))
    json
  }

  def serialize(src: Geometry, typeOfSrc: Type, context: JsonSerializationContext): JsonElement = src match {
    case point:Point =>
      serializeGeometry("Point", point, context)
    case LineString(points) =>
      serializeGeometry("LineString", points, context)
    case Polygon(points) =>
      serializeGeometry("Polygon", points, context)
    case MultiPoint(points) =>
      serializeGeometry("MultiPoint", points, context)
    case MultiLineString(lines) =>
      serializeGeometry("MultiLineString", lines.map(_.points), context)
    case MultiPolygon(polygons) =>
      serializeGeometry("MultiPolygon", polygons.map(_.points), context)
    case feature:Feature =>
      serializeFeature(feature, context)
    case features:FeatureCollection =>
      serializeFeatures(features, context)
    case _ =>
      throw new UnsupportedOperationException
  }

}

class GeoJsonDeserializer extends JsonDeserializer[Data] {

  def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): Data = {
    val o = json.getAsJsonObject
    o.get("type").getAsString match {
      case "Point" =>
        val coordinates = context.deserialize[Array[Double]](o.get("coordinates"), classOf[Array[Double]])
        Point(coordinates(0), coordinates(1))
      case "LineString" =>
        val coordinates = context.deserialize[Array[Array[Double]]](o.get("coordinates"), classOf[Array[Array[Double]]])
        val points = coordinates.map(c => Point(c(0), c(1))).toList
        LineString(points)
      case "Polygon" =>
        val coordinates = context.deserialize[Array[Array[Double]]](o.get("coordinates"), classOf[Array[Array[Double]]])
        val points = coordinates.map(c => Point(c(0), c(1))).toList
        Polygon(points)
      case "MultiPoint" =>
        val coordinates = context.deserialize[Array[Array[Double]]](o.get("coordinates"), classOf[Array[Array[Double]]])
        val points = coordinates.map(c => Point(c(0), c(1))).toList
        MultiPoint(points)
      case "MultiLineString" =>
        val coordinates = context.deserialize[Array[Array[Array[Double]]]](o.get("coordinates"), classOf[Array[Array[Double]]])
        val lines = coordinates.map(array => {
          val points = array.map(c => Point(c(0), c(1))).toList
          LineString(points)
        }).toList
        MultiLineString(lines)
      case "MultiPolygon" =>
        val coordinates = context.deserialize[Array[Array[Array[Double]]]](o.get("coordinates"), classOf[Array[Array[Double]]])
        val lines = coordinates.map(array => {
          val points = array.map(c => Point(c(0), c(1))).toList
          Polygon(points)
        }).toList
        MultiPolygon(lines)
      case "Feature" =>
        val geometry = context.deserialize[Geometry](o.get("geometry"), classOf[Geometry])
        val properties = context.deserialize[java.util.Map[String, Any]](o.get("properties"), classOf[java.util.Map[String, Any]])
        val id = if (o.has("id")) Some(o.get("id").getAsString) else None
        Feature(geometry, properties.asScala.toMap, id)
      case "FeatureCollection" =>
        val features = context.deserialize(o.get("features"), classOf[Feature])
        FeatureCollection(features)
      case _ =>
        throw new JsonParseException("Unknown GeoJson type")
    }
  }

}

object GeoJson extends Format[String] {

  private val gson = new GsonBuilder()
    .registerTypeAdapter(classOf[Point], new GeoJsonSerializer())
    .registerTypeAdapter(classOf[LineString], new GeoJsonSerializer())
    .registerTypeAdapter(classOf[Polygon], new GeoJsonSerializer())
    .registerTypeAdapter(classOf[MultiPoint], new GeoJsonSerializer())
    .registerTypeAdapter(classOf[MultiLineString], new GeoJsonSerializer())
    .registerTypeAdapter(classOf[MultiPolygon], new GeoJsonSerializer())
    .registerTypeAdapter(classOf[Feature], new GeoJsonSerializer())
    .registerTypeAdapter(classOf[FeatureCollection], new GeoJsonSerializer())
    .registerTypeAdapter(classOf[Geometry], new GeoJsonDeserializer())
    .create()

  def encode(o:Data):String = {
    gson.toJson(o)
  }

  def decode(json:String):Data = {
    gson.fromJson(json, classOf[Data])
  }

}
