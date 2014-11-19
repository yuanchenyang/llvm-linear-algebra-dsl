import javax.imageio._
import javax.imageio.stream._
import java.io._
import java.awt.image._

//class Image(val filename: String) {
//  var bi: BufferedImage = ImageIO.read(new File(filename))
//}

object Image {
  def main(args: Array[String]) {
    val bi = ImageIO.read(new File("RasBodik.jpg"))
    println(bi.getWidth)
    println(bi.getHeight)

    //val r = new Matrix[Int](bi.getHeight, bi.getWidth)
    //val g = new Matrix[Int](bi.getHeight, bi.getWidth)
    //val b = new Matrix[Int](bi.getHeight, bi.getWidth)
    val r = new Array[Int](bi.getHeight, bi.getWidth)
    val g = new Array[Int](bi.getHeight, bi.getWidth)
    val b = new Array[Int](bi.getHeight, bi.getWidth)
  }
}
