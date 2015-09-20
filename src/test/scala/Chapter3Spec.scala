import Chapter3._
import org.scalatest._

/**
 * Tests for Chapter2 exercises
 */
class Chapter3Spec extends  UnitSpec {

  "List example" should "show the usage of List" in {
    List.sum(Cons(2, Cons(3, Nil))) should be (5)
    List.product(Cons(2.0, Cons(2.0, Nil))) should be (4.0)

    List(2, 3) should be (Cons(2, Cons(3, Nil)))

    List.sum(List(2, 3)) should be (5)
    List.product(List(2.0, 2.0)) should be (4.0)
  }

  "Exercise 3.1" should "show understanding of match" in {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }

    x should be (3)
  }

  "Exercise 3.2" should "implement tail" in {
    List.tail(Nil) should be (Nil)
    List.tail(List(1,2,3,4,5)) should be (List(2,3,4,5))
  }
}
