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
  def makeArray(w: Int, h: Int): Array[Double] = {
    val a = new Array[Double](w * h + 2)
    a(0) = w
    a(1) = h
    a
  }

  def getIndex(a: Array[Double], x: Int, y: Int) = {
    val w = a(0).toInt
    val h = a(1).toInt
    w * y + x + 2
  }


  def process(inputFileName: String, outputFileName: String) {
    val a = Array(Array(0.11111, 0.11111, 0.11111),
                  Array(0.11111, 0.11111, 0.11111),
                  Array(0.11111, 0.11111, 0.11111))
      //  Array(Array(-1, 0, 1),
      //        Array(-2, 0, 2),
      //        Array(-1, 0, 1))

    val snippet = new DslDriver[Array[Double], Array[Double]] {
      def snippet(input: Rep[Array[Double]]) = {
        def specialized(filterIn: Array[Array[Double]], input: Rep[Array[Double]]) = {
          val w = input(0).toInt
          val h = input(1).toInt
          // Assuming filter is symmetrical
          val padding = (filterIn.length - 1) / 2
          val filter = staticData(filterIn)
          var output = NewArray[Double](input.length)
          output(0) = w
          output(1) = h

          for (y <- (padding until h - padding):Rep[Range]) {
            for (x <- (padding until w - padding):Rep[Range]) {
              for (xx <- (-padding to padding):Range) {
        	for (yy <- (-padding to padding):Range) {
                  val inputIndex = 2 + w * (y + yy) + x + xx
                  val outputIndex = 2 + w * y + x
        	  output(outputIndex) = output(outputIndex) + input(inputIndex) * filter(yy + padding).apply(xx + padding)
        	}
              }
            }
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

    val rm = makeArray(height, width)
    val gm = makeArray(height, width)
    val bm = makeArray(height, width)

    for ( y <- 0 until width) {
      for ( x <- 0 until height) {
        val RGB = bi.getRGB(y, x)
        val r = ((RGB>>16) & 255)
        val g = ((RGB>>8)  & 255)
        val b = ((RGB)     & 255)
        rm(getIndex(rm, x, y)) = r
        gm(getIndex(rm, x, y)) = g
        bm(getIndex(rm, x, y)) = b
      }
    }
    println(snippet.code)
    val rmm = snippet.eval(rm)
    val gmm = snippet.eval(gm)
    val bmm = snippet.eval(bm)
    val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);

    for ( y <- 0 until width) {
      for ( x <- 0 until height) {
        val r = rmm(getIndex(rmm, x, y)).toInt
        val g = gmm(getIndex(rmm, x, y)).toInt
        val b = bmm(getIndex(rmm, x, y)).toInt
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
