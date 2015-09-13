import org.scalatest.{Matchers, FunSpec}
import Chapter2._
/**
 * Tests for Chapter2 exercises
 */
class Chapter2Spec extends FunSpec with Matchers  {

  it("Exercise 2.1: fib sequence must be correct") {
    fib(0) should be (0)
    fib(1) should be (1)
    fib(2) should be (1)
    fib(3) should be (2)
    fib(4) should be (3)
    fib(5) should be (5)
    fib(6) should be (8)
  }

  it("Exercise 2.2: isSorted must be correct") {
    isSorted(Array(1,2,3), (i: Int, j: Int) => j > i) should be (true)
    isSorted(Array(4,2,3), (i: Int, j: Int) => j > i) should be (false)
    isSorted(Array(1,1,3), (i: Int, j: Int) => j >= i) should be (true)
  }

}
