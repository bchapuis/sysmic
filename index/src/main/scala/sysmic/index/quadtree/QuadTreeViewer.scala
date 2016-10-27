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

package sysmic.index.quadtree

import java.awt.{Color, Graphics2D, Stroke}
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import sysmic.geometry.{BBox, Point}
import sysmic.index.quadtree.QuadTree._

/**
  * Created by bchapuis on 15/04/16.
  */
object QuadTreeViewer {

  def draw(file: String, width: Int, height: Int, tree: QuadTree[_, _], query: BBox, selection: List[Entry[_, _]]): Unit = {
    val bi = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g = bi.createGraphics()
    drawShapes(g, Some(tree.root))
    drawTree(g, Some(tree.root))
    drawSelection(g, selection)
    g.setPaint(Color.RED)
    g.drawRect(query.p1.x.toInt, query.p1.y.toInt, query.width.toInt, query.height.toInt)
    val f = new File(file)
    ImageIO.write(bi, "png", f)
  }

  private def drawShapes(g: Graphics2D, node: Option[Node[_, _]]): Unit = {
    if (node.isDefined) {
      node.get.entries.foreach(e =>
            e.key match {
          case p: Point =>
            g.setPaint(Color.BLUE)
            g.drawOval(p.x.toInt, p.y.toInt, 3, 3)
          case r: BBox =>
            g.setPaint(Color.BLUE)
            g.drawRect(r.p1.y.toInt, r.p1.y.toInt, r.width.toInt, r.height.toInt)
          case _ =>
      })
      drawShapes(g, node.get.nw)
      drawShapes(g, node.get.ne)
      drawShapes(g, node.get.sw)
      drawShapes(g, node.get.se)
    }
  }

  def drawSelection(g: Graphics2D, selection: List[Entry[_, _]]) = {
    selection.foreach(e =>
          e.key match {
        case p: Point =>
          g.setPaint(Color.GREEN)
          g.drawOval(p.x.toInt, p.y.toInt, 3, 3)
        case r: BBox =>
          g.setPaint(Color.GREEN)
          g.drawRect(r.p1.x.toInt, r.p1.y.toInt, r.width.toInt, r.height.toInt)
        case _ =>
    })
  }

  private def drawTree(g: Graphics2D, node: Option[Node[_, _]]): Unit = {
    if (node.isDefined) {
      g.setPaint(Color.GRAY)
      g.drawRect(node.get.boundary.p1.x.toInt, node.get.boundary.p1.y.toInt,
                 (node.get.boundary.p2.x - node.get.boundary.p1.x).toInt,
                 (node.get.boundary.p2.y - node.get.boundary.p1.y).toInt)
      drawTree(g, node.get.nw)
      drawTree(g, node.get.ne)
      drawTree(g, node.get.sw)
      drawTree(g, node.get.se)
    }
  }

}
