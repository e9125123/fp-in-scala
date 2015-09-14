/**
 * Exercises from Chapter 2
 */
object Chapter2 {

  /**
   * Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
   * The first two Fibonacci numbers are 0 and 1 . The nth number is always the sum of the
   * previous two—the sequence begins 0, 1, 1, 2, 3, 5 . Your definition should use a
   * local tail-recursive function.
   *
   * def fib(n: Int): Int
   */
  def fib(n: Int): Int = {
    if (n == 0)
      0
    else if (n == 1)
      1
    else
      fib(n - 1) + fib(n - 2)
  }

  /**
   * Implement isSorted , which checks whether an Array[A] is sorted according to a
   * given comparison function:
   *
   * def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean
   */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length < 2)
      true
    else {
      @annotation.tailrec
      def loop(current: A, index: Int): Boolean =
        if (index >= as.length)
          true
        else if (ordered(current, as(index)))
          loop(as(index), index+1)
        else false
      loop(as(0), 1)
    }
  }

  /**
   * Let’s look at another example, currying, which converts a function f of two arguments
   * into a function of one argument that partially applies f . Here again there’s only one
   * implementation that compiles. Write this implementation.
   *
   * def curry[A,B,C](f: (A, B) => C): A => (B => C)
   */
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  /**
   * Implement uncurry , which reverses the transformation of curry . Note that since =>
   * associates to the right, A => (B => C) can be written as A => B => C .
   *
   * def uncurry[A,B,C](f: A => B => C): (A, B) => C
   */
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  /**
   * Implement the higher-order function that composes two functions.
   *
   * def compose[A,B,C](f: B => C, g: A => B): A => C
   */
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}
