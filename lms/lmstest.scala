import javax.imageio._
import javax.imageio.stream._
import java.io._
import java.awt.image._

import org.scalatest.FunSuite
import scala.collection.mutable.Stack
import scala.virtualization.lms.common._

trait Ackermann extends Dsl {
  def a(m: Int): Rep[Int => Int] = fun { (n: Rep[Int]) =>
    generate_comment("ack_"+m) // to navigate the generated code
    if (m==0) n+1
    else if (n==0) a(m-1)(1)
    else a(m-1)(a(m)(n-1))
  }
}

trait Mul extends Dsl {
  def m(a: Int): Rep[Int => Int] = fun { (b: Rep[Int]) =>
    a * b
  }
}

class Matrix {
  var width: Int = 0
  var height: Int = 0
  var data: Array[Int] = new Array[Int](0)

  def this(width: Int, height: Int) = {
    this()
    this.width = width
    this.height = height
    this.data = new Array[Int](width * height)
  }

  def getIndex(x: Int, y: Int): Int = {
    this.width * y + x
  }

  def getItem(x: Int, y: Int): Int = {
    this.data(this.getIndex(x, y))
  }

  def setItem(x: Int, y: Int, item: Int) = {
    this.data(this.getIndex(x, y)) = item
  }
}

object Image {
  def process(inputFileName: String, outputFileName: String) {
    val bi = ImageIO.read(new File(inputFileName))
    val width = bi.getWidth
    val height = bi.getHeight

    val rm = new Matrix(bi.getHeight, bi.getWidth)
    val gm = new Matrix(bi.getHeight, bi.getWidth)
    val bm = new Matrix(bi.getHeight, bi.getWidth)

    for ( y <- 0 until width) {
      for ( x <- 0 until height) {
        val RGB = bi.getRGB(y, x)
        val r = (((RGB>>16) & 255) + 128) % 255
        val g = (((RGB>>8)  & 255) + 128) % 255
        val b = (((RGB)     & 255) + 128) % 255
        rm.setItem(x, y, r)
        gm.setItem(x, y, g)
        bm.setItem(x, y, b)
      }
    }
    val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);

    for ( y <- 0 until width) {
      for ( x <- 0 until height) {
        val r = rm.getItem(x, y)
        val g = gm.getItem(x, y)
        val b = bm.getItem(x, y)
        val rgb = (r << 16) | (g << 8) | b
        img.setRGB(y, x, rgb)
      }
    }
    ImageIO.write(img, "jpg", new File(outputFileName))
  }
}

object Main {
  // def specialize(m: Int): DslDriver[Int,Int] = new DslDriver[Int,Int] with Ackermann {
  //   def snippet(n: Rep[Int]): Rep[Int] = a(m)(n)
  // }

  def specialize(a: Int): DslDriver[Int,Int] = new DslDriver[Int,Int] with Mul {
    def snippet(b: Rep[Int]): Rep[Int] = m(a)(b)
  }

  def main(args: Array[String]) {
    // Specialize Ackermann function to concrete value 2
    //var m = new Matrix(2, 3)
    //println(specialize(0).code)

    Image.process(args(0), args(1))
  }
}
