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
}
