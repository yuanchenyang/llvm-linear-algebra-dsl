import javax.imageio._
import javax.imageio.stream._
import java.io._
import java.awt.image._

import org.scalatest.FunSuite
import scala.collection.mutable.Stack
import scala.util.Random
import scala.util.Random
import scala.virtualization.lms.common._
import Array._

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


object Main {
  def main(args: Array[String]): Unit = {

  def process(inputFileName: String, outputFileName: String) {
    val a =
      Array(Array(-1, 0, 1),
        Array(-2, 0, 2),
        Array(-1, 0, 1))
    
    val snippet = new DslDriver[Array[Array[Int]], Array[Array[Int]]] {
      def snippet(input: Rep[Array[Array[Int]]]) = {
        def specialized(filterIn: Array[Array[Int]], input: Rep[Array[Array[Int]]]) = {
          val h = input.length
          val w = input(0).length
          // Assuming filter is symmetrical
          val padding = (filterIn.length - 1) / 2
          val filter = staticData(filterIn)
          var output = NewArray[Array[Int]](h)
    
          for (i <- (padding until h - padding):Rep[Range]) {
            var row = NewArray[Int](w)
            for (j <- (padding until w - padding):Rep[Range]) {
              for (ii <- (-padding to padding):Range) {
        	for (jj <- (-padding to padding):Range) {
        	  row(j) = row(j) + input(i + unit(ii)).apply(j + unit(jj)) * filter(ii + padding).apply(jj + padding)
        	}
              }
            }
            output(i) = row
          }
          output
        }
        val v1 = specialized(a, input)
        v1
      }
    }
    val bi = ImageIO.read(new File(inputFileName))
    val width = bi.getWidth
    val height = bi.getHeight

    val rm = Array.ofDim[Int](bi.getHeight, bi.getWidth)
    val gm = Array.ofDim[Int](bi.getHeight, bi.getWidth)
    val bm = Array.ofDim[Int](bi.getHeight, bi.getWidth)

    for ( y <- 0 until width) {
      for ( x <- 0 until height) {
        val RGB = bi.getRGB(y, x)
        val r = (((RGB>>16) & 255) + 128) % 255
        val g = (((RGB>>8)  & 255) + 128) % 255
        val b = (((RGB)     & 255) + 128) % 255
        rm(x)(y) = r
        gm(x)(y) = g
        bm(x)(y) = b
      }
    }
    val rmm = snippet.apply(rm)
    val gmm = snippet.apply(gm)
    val bmm = snippet.apply(bm)
    val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);

    for ( y <- 0 until width) {
      for ( x <- 0 until height) {
        val r = rmm(x)(y)
        val g = gmm(x)(y)
        val b = bmm(x)(y)
        val rgb = (r << 16) | (g << 8) | b
        img.setRGB(y, x, rgb)
      }
    }
    ImageIO.write(img, "jpg", new File(outputFileName))
  }

  def main(args: Array[String]) {
    // Specialize Ackermann function to concrete value 2
    //var m = new Matrix(2, 3)
    //println(specialize(0).code)

    process(args(0), args(1))
  }

}
