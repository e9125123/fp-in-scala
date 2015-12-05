/**
 * Code to go with chapter 5 of 'Functional Programming in Scala'
 */
import scala.collection.immutable._

object Chapter5 {

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Empty => List()
      case Cons(h, t) => h() :: t().toList
    }

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    def take(i: Int): Stream[A] = {
      if ( i<= 0) Empty
      else {
        this match {
          case Empty => Empty
          case Cons(h, t) => Cons(h, () => t().take(i - 1))
        }
      }
    }

    def drop(i: Int): Stream[A] = {
      if ( i<= 0) this
      else {
        this match {
          case Empty => Empty
          case Cons(h, t) => t().drop(i-1)
        }
      }
    }
  }
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = {
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    }
  }
}
