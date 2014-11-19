
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


object Main {
//  def specialize(m: Int): DslDriver[Int,Int] = new DslDriver[Int,Int] with Ackermann {
//    def snippet(n: Rep[Int]): Rep[Int] = a(m)(n)
//  }
//
//  def main(args: Array[String]) {
//    // Specialize Ackermann function to concrete value 2
//    println(specialize(2).code)
//  }
  def main(args: Array[String]): Unit = {
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
    println(snippet.code)
  }

}
