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
import scala.math._

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

object Main {
  def makeArray(w: Int, h: Int): Array[Int] = {
    val a = new Array[Int](w * h + 2)
    a(0) = w
    a(1) = h
    a
  }

  def getIndex(a: Array[Int], x: Int, y: Int) = {
    val w = a(0) //.toInt
    val h = a(1) //.toInt
    w * y + x + 2
  }


  def process(inputFileName: String, outputFileName: String) {
    // The two kernels used for convolution. Together, they provide edge
    // detection.
    val a = Array(Array(-1, 0, 1),
                  Array(-2, 0, 2),
                  Array(-1, 0, 1))
    val b = Array(Array(1, 2, 1),
                  Array(0, 0, 0),
                  Array(-1, -2, -1))

    // A new instance of the DslDriver class, which partially evaluates the
    // snippet method.
    val snippet = new DslDriver[Array[Int], Array[Int]] {
      def snippet(input: Rep[Array[Int]]) = {
        def specialized(filterIn: Array[Array[Int]], input: Rep[Array[Int]]) = {
          // filterIn : the convolution kernel, which is partially evaluated
          // input    : The input matrix. Since we can only have one input
          //            argument, we pass in the matrix as an array, with its
          //            dimensions as the first two elements.
          val w = input(0) //.toInt
          val h = input(1) //.toInt
          // Assuming filter is symmetrical
          val padding = (filterIn.length - 1) / 2
          val filter = staticData(filterIn)
          var output = NewArray[Int](input.length)
          output(0) = w
          output(1) = h

          // Main convolution loop. For each pixel in the input image, take the
          // surrounding size(kernel) pixels, multiply each by the corresponding
          // kernel element and sum them together.
          for (y <- (padding until h - padding):Rep[Range]) {
            for (x <- (padding until w - padding):Rep[Range]) {
              for (xx <- (-padding to padding):Range) {
        	for (yy <- (-padding to padding):Range) {
                  // Hackish indexing
                  val inputIndex = 2 + w * (y + yy) + x + xx
                  val outputIndex = 2 + w * y + x
                  output(outputIndex) = output(outputIndex) + input(inputIndex) * filter(yy + padding).apply(xx + padding)
                }
              }
            }
          }
          output
        }
        // Specialize the convolution to each kernel
        val v1 = specialized(a, input)
        val v2 = specialized(b, input)
        // Combine the two resultant matrices together:
        // output[i, j] = sqrt(v1[i, j]^2 + v2[i, j]^2)
        def gradient(v1: Rep[Array[Int]], v2: Rep[Array[Int]]) = {
          val w = v1(0)//.toInt
          var output = NewArray[Int](input.length)
          output(0) = w
          output(1) = v1(1)
          for (y <- (0 until v1(1)/*.toInt*/)) {
            for (x <- (0 until w)) {
              val idx = 2 + y * w + x
              val v1_elt = v1(idx)
              val v2_elt = v2(idx)
              output(idx) = v1_elt * v1_elt + v2_elt * v2_elt
            }
          }
          output
        }
        val output = gradient(v1, v2)
        output
      }
    }

    // IO plumbing
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

    val rmm = snippet.eval(rm)
    val gmm = snippet.eval(gm)
    val bmm = snippet.eval(bm)

    // IO plumbing
    val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
    for ( y <- 0 until width) {
      for ( x <- 0 until height) {
        val r = sqrt(rmm(getIndex(rmm, x, y)).toDouble).toInt
        val g = sqrt(gmm(getIndex(rmm, x, y)).toDouble).toInt
        val b = sqrt(bmm(getIndex(rmm, x, y)).toDouble).toInt
        val rgb = (r << 16) | (g << 8) | b
        img.setRGB(y, x, rgb)
      }
    }
    ImageIO.write(img, "jpg", new File(outputFileName))
    println(snippet.code)
  }

  def main(args: Array[String]) {
    // Specialize Ackermann function to concrete value 2
    //var m = new Matrix(2, 3)
    //println(specialize(0).code)

    process(args(0), args(1))
  }

}
