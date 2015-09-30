import Chapter4._

/**
 * Tests for exercises
 */
class Chapter4Spec extends UnitSpec {

  "Exercise 4.1" should "implement mao" in {
    val s = Some(1)
    s.map(_ + 1) should be(Some(2))
  }

  "Exercise 4.1" should "implement flatMao" in {
    val s = Some(3)
    def f(i: Int) = if (i>2) Some(i) else None
    def g(i: Int) = if (i<2) Some(i) else None

    s.flatMap(f) should be(Some(3))
    s.flatMap(g) should be(None)
  }

  "Exercise 4.1" should "implement getOrElse" in {
    val s = Some(3)
    val n = None

    s.getOrElse(2) should be(3)
    n.getOrElse(2) should be(2)
  }
}
