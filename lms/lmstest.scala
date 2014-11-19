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

class Matrix[T] {
  var x: Int
  var y: Int
  var data: Array[T]

  def this(x: Int, y: Int) = {
    this()
    this.x = x
    this.y = y
    this.data = NewArray[T](x * y)
  }

  def getIndex(x: Int, y: Int): Int {
    this.x * y + this.y
  }

  def getItem(x: Int, y: Int): T {
    this.data[this.getIndex(x, y)]
  }
  def setItem(x: Int, y: Int, item: T) {
    this.data[this.getIndex(x, y)] = item
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
    var m = new Matrix(2, 3)
    //println(specialize(0).code)
  }
}
