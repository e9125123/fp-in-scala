import scala.annotation.tailrec

/**
 * Exercises from Chapter 3
 */
object Chapter3 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[A](head: A, tail: List[A]) extends List[A]

  object List {

    //    def sum(ints: List[Int]): Int = ints match {
    //      case Nil => 0
    //      case Cons(x, xs) => x + sum(xs)
    //    }

    def sum(ints: List[Int]): Int = {
      foldRight(ints, 0)(_ + _)
    }

    //    def product(ds: List[Double]): Double = ds match {
    //      case Nil => 1.0
    //      case Cons(0.0, _) => 0.0
    //      case Cons(h, t) => h * product(t)
    //    }

    def product(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)

    def product2(ds: List[Double]): Double = foldRight(ds, 1.0)((x, y) => {
      if (x == 0.0 || y == 0.0)
        0.0
      else
        x * y
    })

    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }

    /**
     * Implement the function tail for removing the first element of a List . Note that the
     * function takes constant time. What are different choices you could make in your
     * implementation if the List is Nil ? We’ll return to this question in the next chapter.
     */
    def tail[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) => t
    }

    /**
     * Using the same idea, implement the function setHead for replacing the first element
     * of a List with a different value.
     */
    def setHead[A](h: A, l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, t) => Cons(h, t)
    }

    /**
     * Generalize tail to the function drop , which removes the first n elements from a list.
     * Note that this function takes time proportional only to the number of elements being
     * dropped—we don’t need to make a copy of the entire List .
     */
    def drop[A](d: Int, l: List[A]): List[A] = {
      if (d < 1)
        l
      else l match {
        case Nil => Nil
        case Cons(_, t) => drop(d - 1, t)
      }
    }

    /**
     * Implement dropWhile , which removes elements from the List prefix as long as they
     * match a predicate.
     */
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) => {
        if (f(h))
          dropWhile(t, f)
        else
          l
      }
    }

    /**
     * Implement a function, init , that returns a List consisting of
     * all but the last element of a List . So, given List(1,2,3,4) , init will
     * return List(1,2,3) .
     * Why can’t this function be implemented in constant time like tail ?
     */
    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

    /**
     * Compute the length of a list using foldRight .
     * def length[A](as: List[A]): Int
     */
    def length[A](as: List[A]): Int = foldRight(as, 0)((_, c: Int) => c + 1)

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

    /**
     * Our implementation of foldRight is not tail-recursive and will result in a StackOver-flowError for large lists
     * (we say it’s not stack-safe). Convince yourself that this is the case, and then write another general
     * list-recursion function, foldLeft , that is tail-recursive, using the techniques we discussed in the previous chapter.
     */
    @tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

    def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

    def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
      foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

    def append[A](l: List[A], a: List[A]): List[A] = {
      foldRight(l, a)(Cons(_, _))
    }

    def flatMap_[A](l: List[List[A]]): List[A] = {
      foldLeft(l, Nil: List[A])(append(_, _))
      //foldRight(l, Nil: List[A])(append(_,_))
    }

    def mapPlusOne(l: List[Int]): List[Int] = {
      foldRight(l, Nil: List[Int])((a: Int, l: List[Int]) => Cons(a + 1, l))
    }

    def mapDoubleToString(l: List[Double]): List[String] = {
      foldRight(l, Nil: List[String])((a: Double, l: List[String]) => Cons(a.toString, l))
    }

    def map[A, B](as: List[A])(f: A => B): List[B] = {
      foldRight(as, Nil: List[B])((a: A, l: List[B]) => Cons(f(a), l))
    }

    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
      foldRight(as, Nil: List[A])((a: A, l: List[A]) => {
        if (f(a)) Cons(a, l)
        else l
      })
    }

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
      //foldLeft(map(as)(f), Nil: List[B])(append(_, _))
      foldRight(map(as)(f), Nil: List[B])(append(_, _))
    }

    def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
      flatMap(l)(a => if (f(a)) List(a) else Nil)
    }

    def zipWithIndexInt(l: List[Int], i: List[Int]): List[Int] = {
      l match {
        case Nil => Nil
        case Cons(h, t) => {
          i match {
            case Nil => Nil
            case Cons(ih, it) => Cons(h + ih, zipWithIndexInt(t, it))
          }
        }
      }
    }

    def zipWith[A, B, C](l: List[A], i: List[B])(f: (A, B) => C): List[C] = {
      l match {
        case Nil => Nil
        case Cons(h, t) => {
          i match {
            case Nil => Nil
            case Cons(ih, it) => Cons(f(h, ih), zipWith(t, it)(f))
          }
        }
      }
    }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      sup match {
        case Nil => sub match {
          case Nil => true
          case _ => false
        }
        case Cons(h, t) => sub match {
          case Nil => true
          case Cons(sh, st) => {
            if (h == sh)
              hasSubsequence(t, st) || hasSubsequence(t, sub)
            else
              hasSubsequence(t, sub)
          }
        }
      }
    }

  }

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def size[A](t: Tree[A]): Int = {
      def size1(c: Int, t1: Tree[A]): Int = {
        t1 match {
          case Leaf(_) => c+1
          case Branch(l, r) => 1 + size1(c, l) + size1(c, r)
        }
      }
      size1(0, t)
    }

    def maximum(t: Tree[Int]): Int = {
      def m(c: Int, t1: Tree[Int]): Int = {
        t1 match {
          case Leaf(v) => c max v
          case Branch(l, r) => c max (maximum(l) max maximum(r))
        }
      }
      m(0, t)
    }

    def depth(t: Tree[Int]): Int = {
      def m(c: Int, t1: Tree[Int]): Int = {
        t1 match {
          case Leaf(v) => c
          case Branch(l, r) => c + (depth(l) max depth(r))
        }
      }
      m(1, t)
    }

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
      case Leaf(a) => f(a)
      case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

    def sizeViaFold[A](t: Tree[A]): Int = fold(t)(a => 1)(1+_+_)

    def maximumViaFold(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

    def depthViaFold[A](t: Tree[A]): Int = fold(t)(a => 1)((d1,d2) => 1 + (d1 max d2))
  }

}
