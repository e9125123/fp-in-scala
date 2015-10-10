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
    def f(i: Int): Option[Int] = if (i > 2) Some(i) else None
    def g(i: Int) = if (i < 2) Some(i) else None

    s.flatMap(f) should be(Some(3))
    s.flatMap(g) should be(None)
  }

  "Exercise 4.1" should "implement getOrElse" in {
    val s = Some(3)
    val n = None

    s.getOrElse(2) should be(3)
    n.getOrElse(2) should be(2)
  }

  "Exercise 4.1" should "implement orElse" in {
    val s = Some(3)
    val n = None

    s.orElse(s) should be(s)
    n.orElse(s) should be(s)
  }

  "Exercise 4.1" should "implement filter" in {
    val s = Some(3)
    val n = None

    s.filter((i: Int) => i > 2) should be(s)
    s.filter((i: Int) => i < 2) should be(None)
  }

  "Exercise 4.2" should "implement variance" in {
    val s = Seq(1.0, 2.0, 3.0)
    val n: Seq[Double] = Seq()

    variance(s) should be(Some(2.0 / 3.0))
    variance(n) should be(None)
  }

  "Exercise 4.4" should "implement sequence" in {
    sequence(List(Some(1), Some(2))) should be(Some(List(1, 2)))
    sequence_1(List(Some(1), Some(2))) should be(Some(List(1, 2)))
    sequence_2(List(Some(1), Some(2))) should be(Some(List(1, 2)))
  }

  "Exercise 4.5" should "implement traverse" in {
    def f(i: Int): Option[Int] = if (i < 2) None else Some(i)
    traverse(List(1, 2))(f) should be(None)
  }

  "Exercise 4.6" should "implement Either" in {
    val r1 : Either[String, Int] = Right(1)
    val l1 = Left("left 1")

    r1.map(_ => "is an int") should be(Right("is an int"))
    r1.flatMap(x => Right(x)) should be(Right(1))
    l1.orElse(r1) should be(r1)
    r1.map2(l1: Either[String, Int])((_, _)=>"mapped") should be(Left("left 1"))
  }

  "Exercise 4.7" should "implement traverse and sequence" in {
    val r1 : Either[String, Int] = Right(1)
    val l1 : Either[String, Int] = Left("left 1")

    val es: List[Either[String, Int]] = List(Right(1), Right(2))
    val es1 = Left("error") :: es

    Either.sequence(es) should be(Right(List(1,2)))
    Either.sequence(es1) should be(Left("error"))
  }

  "Exercise 4.8" should "accumulate errors for validation" in {
    mkPerson("p1", 10) should be(Right(Person(Name("p1"), Age(10))))
    mkPerson("", 10) should be(Left("Name is empty."))
    mkPerson("a", -1) should be(Left("Age is out of range."))
  }
}
