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
}
