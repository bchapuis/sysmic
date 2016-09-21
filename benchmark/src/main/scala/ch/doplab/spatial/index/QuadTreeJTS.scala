///*
// * Licensed under the Apache License, Version 2.0 (the "License");
// * you may not use this file except in compliance with the License.
// * You may obtain a copy of the License at
// *
// *     http://www.apache.org/licenses/LICENSE-2.0
// *
// * Unless required by applicable law or agreed to in writing, software
// * distributed under the License is distributed on an "AS IS" BASIS,
// * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// * See the License for the specific language governing permissions and
// * limitations under the License.
// */
//
//package ch.doplab.spatial.index
//
//import org.openjdk.jmh.annotations._
//
//import scala.util.Random
//
///**
//  * Created by bchapuis on 29/04/16.
//  */
//@State(Scope.Thread)
//class QuadTreeJTS {
//
//  val r = new Random(1)
//  val w = 500
//  val h = 500
//  val n = 10000
//
//  val geometries = for {
//    i <- 1 to n
//    x1 = r.nextDouble() * w
//    y1 = r.nextDouble() * h
//    x2 = r.nextDouble() * 20 + 1 + x1
//    y2 = r.nextDouble() * 20 + 1 + y1
//  } yield (new Envelope(x1, y1, x2, y2), i)
//
//  val tree = {
//    val t = new Quadtree()
//    for (g <- geometries) {
//      t.insert(g._1, g._2)
//    }
//    t
//  }
//
//  @Benchmark
//  @BenchmarkMode(Array(Mode.AverageTime))
//  def insert() = {
//    val tree = new Quadtree()
//    for (geometry <- geometries) {
//      tree.insert(geometry._1, geometry._2)
//    }
//  }
//
//  @Benchmark
//  @BenchmarkMode(Array(Mode.AverageTime))
//  def search():Unit = {
//    for (geometry <- geometries) {
//      tree.query(geometry._1)
//    }
//  }
//
//}
