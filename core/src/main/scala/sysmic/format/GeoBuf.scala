package sysmic.format

import sysmic.geometry._
import sysmic.protobuf.Geobuf

import sysmic.protobuf.Geobuf.Data.DataTypeCase._
import sysmic.protobuf.Geobuf.Data.Value.ValueTypeCase._
import sysmic.protobuf.Geobuf.Data.Geometry.Type._

import scala.collection.JavaConversions._

object GeoBuf extends Format[Array[Byte]] {

  val precision = 1e6.toInt

  def decodeCoords(geometry:Geobuf.Data.Geometry):List[Point] = {
    assert(geometry.getCoordsCount >= 2)
    val coords = geometry.getCoordsList
    val points = for (i <- 0 until coords.size by 2) yield {
      val x = coords.get(i).toDouble / precision
      val y = coords.get(i + 1).toDouble / precision
      Point(x, y)
    }
    points.toList
  }

  def decodePoint(geometry:Geobuf.Data.Geometry):Point = {
    assert(geometry.getType == POINT)
    assert(geometry.getCoordsCount == 2)
    val coords = geometry.getCoordsList
    val x = coords.get(0).toDouble / precision
    val y = coords.get(1).toDouble / precision
    Point(x, y)
  }

  def decodeLineString(geometry:Geobuf.Data.Geometry):LineString = {
    assert(geometry.getType == LINESTRING)
    val points = decodeCoords(geometry)
    LineString(points)
  }

  def decodePolygon(geometry:Geobuf.Data.Geometry):Polygon = {
    assert(geometry.getType == POLYGON)
    val points = decodeCoords(geometry)
    Polygon(points)
  }

  def decodeMultiPoint(geometry:Geobuf.Data.Geometry):MultiPoint = {
    assert(geometry.getType == MULTIPOINT)
    val points = geometry.getGeometriesList.map(geom => decodePoint(geom)).toList
    MultiPoint(points)
  }

  def decodeMultiLineString(geometry:Geobuf.Data.Geometry):MultiLineString = {
    assert(geometry.getType == MULTILINESTRING)
    val lines = geometry.getGeometriesList.map(geom => decodeLineString(geom)).toList
    MultiLineString(lines)
  }

  def decodeMultiPolygon(geometry:Geobuf.Data.Geometry):MultiPolygon = {
    assert(geometry.getType == MULTIPOLYGON)
    val polygons = geometry.getGeometriesList.map(geom => decodePolygon(geom)).toList
    MultiPolygon(polygons)
  }

  def decodeGeometry(geometry:Geobuf.Data.Geometry):Geometry = {
    geometry.getType match {
      case POINT => decodePoint(geometry)
      case LINESTRING => decodeLineString(geometry)
      case POLYGON => decodePolygon(geometry)
      case MULTIPOINT => decodeMultiPoint(geometry)
      case MULTILINESTRING => decodeMultiLineString(geometry)
      case MULTIPOLYGON => decodeMultiPolygon(geometry)
    }
  }

  def decodeFeature(data:Geobuf.Data, feature:Geobuf.Data.Feature):Feature = {
    val id = if (feature.hasId) Some(feature.getId) else None
    val geometry = feature.getGeometry
    val properties = feature.getPropertiesList
    val map = for (property <- properties) yield {
      val i = feature.getProperties(property)
      val k = data.getKeys(i)
      val o = feature.getValues(i)
      val v = o.getValueTypeCase match {
        case STRING_VALUE => o.getStringValue
        case DOUBLE_VALUE => o.getDoubleValue
        case POS_INT_VALUE => o.getPosIntValue
        case NEG_INT_VALUE => o.getNegIntValue
        case BOOL_VALUE => o.getBoolValue
        case JSON_VALUE => o.getJsonValue
        case VALUETYPE_NOT_SET => Nil
      }
      (k, v)
    }
    Feature(decodeGeometry(geometry), map.toMap, id)
  }

  def decodeFeatureCollection(data:Geobuf.Data):FeatureCollection = {
    assert(data.getDataTypeCase == FEATURE_COLLECTION)
    val featureCollection = data.getFeatureCollection
    val features = featureCollection.getFeaturesList.map(f => decodeFeature(data, f)).toList
    FeatureCollection(features)
  }

  override def decode(bytes:Array[Byte]):Data = {
    val data = Geobuf.Data.parseFrom(bytes)
    val index = data.getKeysList
    data.getDataTypeCase match {
      case GEOMETRY =>
        decodeGeometry(data.getGeometry)
      case FEATURE =>
        decodeFeature(data, data.getFeature)
      case FEATURE_COLLECTION =>
        decodeFeatureCollection(data)
    }
  }

  def encodeCoords(builder:Geobuf.Data.Geometry.Builder, point:Point):Geobuf.Data.Geometry.Builder = {
    builder
      .addCoords((point.x * precision).toLong)
      .addCoords((point.y * precision).toLong)
  }

  def encodeCoords(builder:Geobuf.Data.Geometry.Builder, points:List[Point]):Geobuf.Data.Geometry.Builder = {
    points.foldLeft(builder)((b, p) => encodeCoords(b, p))
  }

  def encodePoint(point:Point):Geobuf.Data.Geometry = {
    val builder = Geobuf.Data.Geometry.newBuilder()
    encodeCoords(builder, point)
      .setType(POINT)
      .build()
  }

  def encodeLineString(lineString:LineString):Geobuf.Data.Geometry = {
    val builder = Geobuf.Data.Geometry.newBuilder()
    encodeCoords(builder, lineString.points)
      .setType(LINESTRING)
      .build()
  }

  def encodePolygon(polygon:Polygon):Geobuf.Data.Geometry = {
    val builder = Geobuf.Data.Geometry.newBuilder()
    encodeCoords(builder, polygon.points)
      .setType(POLYGON)
      .build()
  }

  def encodeMultiPoint(multiPoint:MultiPoint):Geobuf.Data.Geometry = {
    val geometries = multiPoint.points.map(point => encodePoint(point))
    Geobuf.Data.Geometry.newBuilder()
      .setType(MULTIPOINT)
      .addAllGeometries(geometries)
      .build()
  }

  def encodeMultiLineString(multiLineString:MultiLineString):Geobuf.Data.Geometry = {
    val geometries = multiLineString.lines.map(line => encodeLineString(line))
    Geobuf.Data.Geometry.newBuilder()
      .setType(MULTILINESTRING)
      .addAllGeometries(geometries)
      .build()
  }

  def encodeMultiPolygon(multiLineString:MultiPolygon):Geobuf.Data.Geometry = {
    val geometries = multiLineString.polygons.map(polygon => encodePolygon(polygon))
    Geobuf.Data.Geometry.newBuilder()
      .setType(MULTIPOLYGON)
      .addAllGeometries(geometries)
      .build()
  }

  def encodeGeometry(geometry:Geometry):Geobuf.Data.Geometry = geometry match {
    case point:Point => encodePoint(point)
    case lineString:LineString => encodeLineString(lineString)
    case polygon:Polygon => encodePolygon(polygon)
    case multiPoint:MultiPoint => encodeMultiPoint(multiPoint)
    case multiLineString:MultiLineString => encodeMultiLineString(multiLineString)
    case multiPolygon:MultiPolygon => encodeMultiPolygon(multiPolygon)
  }

  def encodeGeometryData(geometry:Geometry):Geobuf.Data = {
    Geobuf.Data.newBuilder()
      .setPrecision(precision)
      .setGeometry(encodeGeometry(geometry))
      .build()
  }

  def encodeProperty(builder:Geobuf.Data.Feature.Builder, index:Map[String, Int], key:String, value:Any):Geobuf.Data.Feature.Builder = {
    builder.addProperties(index.get(key).get)
    val vb = Geobuf.Data.Value.newBuilder()
    value match {
      case s:String =>
        builder.addValues(vb.setStringValue(s).build())
      case d:Double =>
        builder.addValues(vb.setDoubleValue(d).build())
      case b:Boolean =>
        builder.addValues(vb.setBoolValue(b).build())
      case i:Int =>
        if (i >= 0) {
          builder.addValues(vb.setPosIntValue(i).build())
        } else {
          builder.addValues(vb.setNegIntValue(i).build())
        }
      case l:Long =>
        if (l >= 0) {
          builder.addValues(vb.setPosIntValue(l).build())
        } else {
          builder.addValues(vb.setNegIntValue(l).build())
        }
    }
  }

  def encodeFeature(index:Map[String, Int], feature:Feature):Geobuf.Data.Feature = {
    val builder = feature.id match {
      case Some(id) => Geobuf.Data.Feature.newBuilder().setId(id)
      case None => Geobuf.Data.Feature.newBuilder()
    }
    feature.properties
      .foldLeft(builder)((b, p) => encodeProperty(b, index, p._1, p._2))
      .setGeometry(encodeGeometry(feature.geometry))
      .build()
  }

  def encodeFeatureData(feature:Feature):Geobuf.Data = {
    val keys = feature.properties.map(_._1).toSet
    val index = keys.zipWithIndex.toMap
    Geobuf.Data.newBuilder()
      .addAllKeys(index.keySet)
      .setPrecision(precision)
      .setFeature(encodeFeature(index, feature))
      .build()
  }

  def encodeFeatureCollection(index:Map[String, Int], features:List[Feature]):Geobuf.Data.FeatureCollection = {
    Geobuf.Data.FeatureCollection.newBuilder()
      .addAllFeatures(features.map(f => encodeFeature(index, f)))
      .build()
  }

  def encodeFeatureCollectionData(featureCollection: FeatureCollection):Geobuf.Data = {
    val keys = featureCollection.features.flatMap(_.properties.map(_._1)).toSet
    val index = keys.zipWithIndex.toMap
    Geobuf.Data.newBuilder()
      .setPrecision(precision)
      .addAllKeys(index.keySet)
      .setFeatureCollection(encodeFeatureCollection(index, featureCollection.features))
      .build()
  }

  def encodeData(data: Data):Geobuf.Data = data match {
    case geometry:Geometry => encodeGeometryData(geometry)
    case feature:Feature => encodeFeatureData(feature)
    case featureCollection:FeatureCollection => encodeFeatureCollectionData(featureCollection)
  }

  def encode(data: Data): Array[Byte] = {
    encodeData(data).toByteArray
  }

}