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

package sysmic.benchmark

import sysmic.index.quadtree.RectangleQuadTree
import org.openjdk.jmh.annotations.{BenchmarkMode, _}
import sysmic.geometry.{BBox, Point}
import sysmic.index.quadtree.QuadTree.Node

import scala.util.Random

/**
  * Created by bchapuis on 03/06/16.
  */
@State(Scope.Thread)
class QuadTreeBenchmark {

  val r = new Random(1)
  val w = 500
  val h = 500
  val n = 10000

  val geometries = for {
    i <- 1 to n
    x1 = r.nextDouble() * w
    y1 = r.nextDouble() * h
    x2 = r.nextDouble() * 20 + x1
    y2 = r.nextDouble() * 20 + y1
  } yield (BBox(Point(x1, y1), Point(x2, y2)), i)

  val tree = geometries.foldLeft(
      new RectangleQuadTree[Int](Node[BBox, Int](BBox(Point(0, 0), Point(w + 20, h + 20))), 10))((t, g) =>
        t.insert(g._1, g._2))

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def insert() = {
    var tree = new RectangleQuadTree[Int](Node[BBox, Int](BBox(Point(0, 0), Point(w, h))), 10)
    for (geometry <- geometries) {
      tree = tree.insert(geometry._1, geometry._2)
    }
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  def search() = {
    for (geometry <- geometries) {
      tree.search(geometry._1)
    }
  }

}
