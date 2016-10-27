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

import java.lang.reflect.{ParameterizedType, Type}

import sysmic.geometry._
import com.google.gson._
import com.google.gson.reflect.TypeToken
import sun.reflect.generics.reflectiveObjects.ParameterizedTypeImpl

import scala.collection.JavaConverters._

class ListAdapter extends JsonSerializer[List[_]] with JsonDeserializer[List[_]] {
  import scala.collection.JavaConverters._

  override def serialize(src: List[_], typeOfSrc: Type, context: JsonSerializationContext): JsonElement = {
    val p = scalaListTypeToJava(typeOfSrc.asInstanceOf[ParameterizedType])
    context.serialize(src.asInstanceOf[List[Any]].asJava, p)
  }

  override def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): List[_] = {
    val p = scalaListTypeToJava(typeOfT.asInstanceOf[ParameterizedType])
    val javaList: java.util.List[_ <: Any] = context.deserialize(json, p)
    javaList.asScala.toList
  }

  private def scalaListTypeToJava(t: ParameterizedType): ParameterizedType = {
    ParameterizedTypeImpl.make(classOf[java.util.List[_]], t.getActualTypeArguments, null)
  }

}

class MapAdapter[A, B] extends JsonSerializer[Map[A,B]] with JsonDeserializer[Map[A,B]] {

  override def serialize(src: Map[A,B], typeOfSrc: Type, context: JsonSerializationContext): JsonElement = {
    context.serialize(src.asInstanceOf[Map[A,B]].asJava)
  }

  override def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): Map[A,B] = {
    context.deserialize[java.util.Map[A,B]](json, classOf[java.util.Map[A,B]]).asScala.toMap
  }

}

class PointAdapter extends JsonSerializer[Point] with JsonDeserializer[Point] {

  override def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): Point = {
    val jsonObject = json.getAsJsonObject
    val coordinates = context.deserialize[Array[Double]](jsonObject.get("coordinates"), classOf[Array[Double]])
    Point(coordinates(0), coordinates(1))
  }

  override def serialize(src: Point, typeOfSrc: Type, context: JsonSerializationContext): JsonElement = {
    val json = new JsonObject()
    json.addProperty("type", "Point")
    val coordinates = Array(src.x, src.y)
    json.add("coordinates", context.serialize(coordinates))
    json
  }

}

class LineStringAdapter extends JsonSerializer[LineString] with JsonDeserializer[LineString] {

  override def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): LineString = {
    val jsonObject = json.getAsJsonObject
    val coordinates = context.deserialize[Array[Array[Double]]](jsonObject.get("coordinates"), classOf[Array[Array[Double]]])
    val points = coordinates.map(c => Point(c(0), c(1))).toList
    LineString(points)
  }

  override def serialize(src: LineString, typeOfSrc: Type, context: JsonSerializationContext): JsonElement = {
    val json = new JsonObject()
    json.addProperty("type", "LineString")
    val coordinates = src.points.map(p => Array(p.x, p.y)).toArray
    json.add("coordinates", context.serialize(coordinates))
    json
  }

}

class PolygonAdapter extends JsonSerializer[Polygon] with JsonDeserializer[Polygon] {

  override def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): Polygon = {
    val jsonObject = json.getAsJsonObject
    val coordinates = context.deserialize[Array[Array[Double]]](jsonObject.get("coordinates"), classOf[Array[Array[Double]]])
    val points = coordinates.map(c => Point(c(0), c(1))).toList
    Polygon(points)
  }

  override def serialize(src: Polygon, typeOfSrc: Type, context: JsonSerializationContext): JsonElement = {
    val json = new JsonObject()
    json.addProperty("type", "Polygon")
    val coordinates = src.points.map(p => Array(p.x, p.y)).toArray
    json.add("coordinates", context.serialize(coordinates))
    json
  }

}

class MultiPointAdapter extends JsonSerializer[MultiPoint] with JsonDeserializer[MultiPoint] {

  override def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): MultiPoint = {
    val jsonObject = json.getAsJsonObject
    val coordinates = context.deserialize[Array[Array[Double]]](jsonObject.get("coordinates"), classOf[Array[Array[Double]]])
    val points = coordinates.map(c => Point(c(0), c(1))).toList
    MultiPoint(points)
  }

  override def serialize(src: MultiPoint, typeOfSrc: Type, context: JsonSerializationContext): JsonElement = {
    val json = new JsonObject()
    json.addProperty("type", "MultiPoint")
    val coordinates = src.points.map(p => Array(p.x, p.y)).toArray
    json.add("coordinates", context.serialize(coordinates))
    json
  }

}

class MultiLineStringAdapter extends JsonSerializer[MultiLineString] with JsonDeserializer[MultiLineString] {

  override def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): MultiLineString = {
    val jsonObject = json.getAsJsonObject
    val coordinates = context.deserialize[Array[Array[Array[Double]]]](jsonObject.get("coordinates"), classOf[Array[Array[Array[Double]]]])
    val lines = coordinates.map(array => {
      val points = array.map(c => Point(c(0), c(1))).toList
      LineString(points)
    }).toList
    MultiLineString(lines)
  }

  override def serialize(src: MultiLineString, typeOfSrc: Type, context: JsonSerializationContext): JsonElement = {
    val json = new JsonObject()
    json.addProperty("type", "MultiLineString")
    val coordinates = src.lines.map(l => l.points.map(p => Array(p.x, p.y)))
    val listType = new TypeToken[List[List[Array[Double]]]] {}.getType
    json.add("coordinates", context.serialize(coordinates, listType))
    json
  }

}

class MultiPolygonAdapter extends JsonSerializer[MultiPolygon] with JsonDeserializer[MultiPolygon] {

  override def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): MultiPolygon = {
    val jsonObject = json.getAsJsonObject
    val coordinates = context.deserialize[Array[Array[Array[Double]]]](jsonObject.get("coordinates"), classOf[Array[Array[Array[Double]]]])
    val lines = coordinates.map(array => {
      val points = array.map(c => Point(c(0), c(1))).toList
      Polygon(points)
    }).toList
    MultiPolygon(lines)
  }

  override def serialize(src: MultiPolygon, typeOfSrc: Type, context: JsonSerializationContext): JsonElement = {
    val json = new JsonObject()
    json.addProperty("type", "MultiPolygon")
    val coordinates = src.polygons.map(l => l.points.map(p => Array(p.x, p.y)))
    val listType = new TypeToken[List[List[Array[Double]]]] {}.getType
    json.add("coordinates", context.serialize(coordinates, listType))
    json
  }

}

class GeometryAdapter extends JsonSerializer[Geometry] with JsonDeserializer[Geometry] {

  override def serialize(src: Geometry, typeOfSrc: Type, context: JsonSerializationContext): JsonElement = src match {
    case g:Point => context.serialize(g, classOf[Point])
    case g:LineString => context.serialize(g, classOf[LineString])
    case g:Polygon => context.serialize(g, classOf[Polygon])
    case g:MultiPoint => context.serialize(g, classOf[MultiPoint])
    case g:MultiLineString => context.serialize(g, classOf[MultiLineString])
    case g:MultiPolygon => context.serialize(g, classOf[MultiPolygon])
  }

  override def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext):Geometry = {
    json.getAsJsonObject.get("type").getAsString match {
      case "Point" => context.deserialize[Point](json, classOf[Point])
      case "LineString" => context.deserialize[LineString](json, classOf[LineString])
      case "Polygon" => context.deserialize[Polygon](json, classOf[Polygon])
      case "MultiPoint" => context.deserialize[MultiPoint](json, classOf[MultiPoint])
      case "MultiLineString" => context.deserialize[MultiLineString](json, classOf[MultiLineString])
      case "MultiPolygon" => context.deserialize[MultiPolygon](json, classOf[MultiPolygon])
      case geoJsonType => throw new JsonParseException(s"Unknown GeoJson type: ${geoJsonType.toString}")
    }
  }

}

class FeatureAdapter extends JsonSerializer[Feature] with JsonDeserializer[Feature] {

  override def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): Feature = {
    val jsonObject = json.getAsJsonObject
    val geometry = context.deserialize[SpatialData](jsonObject.get("geometry"), classOf[SpatialData]).asInstanceOf[Geometry]
    val properties = context.deserialize[java.util.Map[String, Any]](jsonObject.get("properties"), classOf[java.util.Map[String, Any]])
    val id = if (jsonObject.has("id")) Some(jsonObject.get("id").getAsString) else None
    Feature(id, geometry, properties.asScala.toMap)
  }

  override def serialize(src: Feature, typeOfSrc: Type, context: JsonSerializationContext): JsonElement = {
    val json = new JsonObject()
    json.addProperty("type", "Feature")
    if (!src.id.isEmpty) {
      json.addProperty("id", src.id.get)
    }
    if (!src.properties.isEmpty) {
      val properties = new JsonObject()
      json.add("properties", context.serialize(src.properties.asJava))
    }
    json.add("geometry", context.serialize(src.geometry))
    json
  }
}

class FeatureCollectionAdapter extends JsonSerializer[FeatureCollection] with JsonDeserializer[FeatureCollection] {

  override def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): FeatureCollection = {
    val jsonObject = json.getAsJsonObject
    val jsonArray = jsonObject.get("features").getAsJsonArray
    val features = for (i <- 0 until jsonArray.size()) yield {
      context.deserialize[SpatialData](jsonArray.get(i), classOf[SpatialData]).asInstanceOf[Feature]
    }
    FeatureCollection(features.toList)
  }

  override def serialize(src: FeatureCollection, typeOfSrc: Type, context: JsonSerializationContext): JsonElement = {
    val json = new JsonObject()
    json.addProperty("type", "FeatureCollection")
    json.add("features", context.serialize(src.features.toArray))
    json
  }
}

class DataAdapter extends JsonSerializer[SpatialData] with JsonDeserializer[SpatialData] {

  override def serialize(src: SpatialData, typeOfSrc: Type, context: JsonSerializationContext): JsonElement = src match {
    case g:Point => context.serialize(g, classOf[Point])
    case g:LineString => context.serialize(g, classOf[LineString])
    case g:Polygon => context.serialize(g, classOf[Polygon])
    case g:MultiPoint => context.serialize(g, classOf[MultiPoint])
    case g:MultiLineString => context.serialize(g, classOf[MultiLineString])
    case g:MultiPolygon => context.serialize(g, classOf[MultiPolygon])
    case g:Feature => context.serialize(g, classOf[Feature])
    case g:FeatureCollection => context.serialize(g, classOf[FeatureCollection])
  }

  override def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): SpatialData = {
    json.getAsJsonObject.get("type").getAsString match {
      case "Point" => context.deserialize[Point](json, classOf[Point])
      case "LineString" => context.deserialize[LineString](json, classOf[LineString])
      case "Polygon" => context.deserialize[Polygon](json, classOf[Polygon])
      case "MultiPoint" => context.deserialize[MultiPoint](json, classOf[MultiPoint])
      case "MultiLineString" => context.deserialize[MultiLineString](json, classOf[MultiLineString])
      case "MultiPolygon" => context.deserialize[MultiPolygon](json, classOf[MultiPolygon])
      case "Feature" => context.deserialize[Feature](json, classOf[Feature])
      case "FeatureCollection" => context.deserialize[FeatureCollection](json, classOf[FeatureCollection])
      case geoJsonType => throw new JsonParseException(s"Unknown GeoJson type: ${geoJsonType.toString}")
    }
  }

}

object GeoJson extends Format[String, SpatialData] {

  val gsonBuilder = new GsonBuilder()
    .registerTypeHierarchyAdapter(classOf[List[_]], new ListAdapter())
    .registerTypeAdapter(classOf[Map[String, Any]], new MapAdapter[String, Any]())
    .registerTypeAdapter(classOf[Point], new PointAdapter())
    .registerTypeAdapter(classOf[LineString], new LineStringAdapter())
    .registerTypeAdapter(classOf[Polygon], new PolygonAdapter())
    .registerTypeAdapter(classOf[MultiPoint], new MultiPointAdapter())
    .registerTypeAdapter(classOf[MultiLineString], new MultiLineStringAdapter())
    .registerTypeAdapter(classOf[MultiPolygon], new MultiPolygonAdapter())
    .registerTypeAdapter(classOf[Geometry], new GeometryAdapter())
    .registerTypeAdapter(classOf[Feature], new FeatureAdapter())
    .registerTypeAdapter(classOf[FeatureCollection], new FeatureCollectionAdapter())
    .registerTypeAdapter(classOf[SpatialData], new DataAdapter())

  val gsonFormat = gsonBuilder.create()

  def encode(o:SpatialData):String = {
    gsonFormat.toJson(o)
  }

  def decode(json:String):SpatialData = {
    gsonFormat.fromJson(json, classOf[SpatialData])
  }

}
