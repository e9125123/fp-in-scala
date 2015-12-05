import Chapter5._

/**
  * Tests for exercises
  */
class Chapter5Spec extends UnitSpec {

  /**
    * Write a function to convert a Stream to a List , which will force its evaluation and let
    * you look at it in the REPL . You can convert to the regular List type in the standard
    * library.
    */
  "Exercise 5.1" should "implement toList" in {
    val s = Stream(1, 2, 3)
    s.toList  should be(List(1, 2, 3))
  }

  /**
    * Write the function take(n) for returning the first n elements of a Stream , and
    * drop(n) for skipping the first n elements of a Stream .
    */
  "Exercise 5.2" should "implement take and drop" in {
    val s = Stream(1, 2, 3, 4)
    s.take(2).toList  should be(List(1, 2))
    s.drop(2).toList  should be(List(3, 4))

    // TODO: should also work for stream equal??
//    s.take(2) should be(Stream(1, 2))
//    s.drop(2) should be(Stream(3, 4))
  }
}
