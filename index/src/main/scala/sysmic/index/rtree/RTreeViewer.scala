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

package sysmic.index.rtree

import java.awt.{Color, Graphics2D}
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import sysmic.index.rtree.RTree._

/**
  * Created by bchapuis on 14/04/16.
  */
object RTreeViewer {

  def draw(file: String, width: Int, height: Int, tree: RTree[_]): Unit = {
    val bi = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g = bi.createGraphics()
    draw(g, tree.root)
    val f = new File(file)
    ImageIO.write(bi, "png", f)
  }

  def draw(g: Graphics2D, node: Node): Unit = {
    node.children.foreach(e => {
      if (!e.value.isInstanceOf[Node]) {
        g.setPaint(Color.RED)
        drawRectangle(g, e.mbb)
      } else {
        g.setPaint(Color.BLUE)
        drawRectangle(g, e.mbb)
      }
      if (e.value.isInstanceOf[Node]) {
        draw(g, e.value.asInstanceOf[Node])
      }
    })
  }

  def drawRectangle(g: Graphics2D, box: MBB) = {
    g.drawRect(box.x1.toInt,
               box.y1.toInt,
               (box.x2 - box.x1).toInt,
               (box.y2 - box.y1).toInt)
  }

}
