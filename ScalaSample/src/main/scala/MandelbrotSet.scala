import java.awt.image._
import java.awt._
import javax.imageio._
import java.io._

object MandelbrotSet {

  val MAX_ITERATIONS = 1000

  def generate(width: Int, height: Int, xStart: Double, yStart: Double, step: Double) = {

    // Generate pixel positions.
    val pixels = (0 until width).flatMap { xpix =>
      (0 until height).map { ypix => (xpix, ypix) }
    }

    // Add pixel color based on escape speed and generate an image.
    pixels
      .par
      .map { case (xp,yp) =>
        val x = xp * step + xStart
        val y = (height - yp) * step + yStart
        (xp, yp, getColor(escapeRatio(x, y)).getRGB())
      }
      .foldLeft(new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)) { case (img, (x, y, rgb)) =>
        img.setRGB(x, y, rgb)
        img
      }
  }

  def getColor(escapeRatio: Double): Color = {
    val lvl = 255 - (escapeRatio * 255).toInt
    new Color(lvl, lvl, lvl)
  }

  def escapeRatio(x0: Double, y0: Double): Double = {
    Iterator
      .iterate(0, 0.0, 0.0) { case (iter, x, y) =>
        val nx = x * x - y * y + x0
        val ny = 2 * x * y + y0
        (iter+1, nx, ny)
      }
      .takeWhile { case (iter, x, y) =>
        iter <= MAX_ITERATIONS && (x*x + y*y) < 4
      }
      .map { case (iter, x, y) => iter }
      .reduceLeft { (a, b) => b } / MAX_ITERATIONS.toDouble
  }

  def main(args: Array[String]): Unit = {
    val st = System.currentTimeMillis
    val img = generate(2048, (2048 * 2.0 / 3.5).toInt, -2.5, -1, 3.5 / 2048.0)
    val et = System.currentTimeMillis - st
    println("Generated image in " + et + " ms.")
    ImageIO.write(img, "png", new File("mandelbrot.png"))
  }
}
