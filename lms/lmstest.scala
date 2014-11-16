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

object Main {
  def specialize(m: Int): DslDriver[Int,Int] = new DslDriver[Int,Int] with Ackermann {
    def snippet(n: Rep[Int]): Rep[Int] = a(m)(n)
  }

  def main(args: Array[String]) {
    // Specialize Ackermann function to concrete value 2
    println(specialize(2).code)
  }
}
