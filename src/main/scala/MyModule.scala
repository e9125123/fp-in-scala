// A comment!
/* Another comment */
/** A documentation comment */
object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  def factorial(n: Int): Int = {
    /**
     * An inner function, or local definition .
      It’s common in Scala to write
      functions that are local to the body
    of another function. In functional
      programming, we shouldn’t consider
      this a bigger deal than local integers
    or strings.
     */
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)
    go(n, 1)

  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
    println(formatFactorial(5))
}