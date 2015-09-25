import Chapter3._

/**
 * Tests for Chapter2 exercises
 */
class Chapter3Spec extends UnitSpec {

  "List example" should "show the usage of List" in {
    List.sum(Cons(2, Cons(3, Nil))) should be(5)
    List.product(Cons(2.0, Cons(2.0, Nil))) should be(4.0)

    List(2, 3) should be(Cons(2, Cons(3, Nil)))

    List.sum(List(2, 3)) should be(5)
    List.product(List(2.0, 2.0)) should be(4.0)
  }

  "Exercise 3.1" should "show understanding of match" in {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }

    x should be(3)
  }

  "Exercise 3.2" should "implement tail" in {
    List.tail(Nil) should be(Nil)
    List.tail(List(1, 2, 3, 4, 5)) should be(List(2, 3, 4, 5))
  }

  "Exercise 3.3" should "implement the function setHead for replacing the first element\nof a List with a different value." in {
    val l = List(1, 2, 3)
    List.setHead(2, l) should be(List(2, 2, 3))
  }

  "Exercise 3.4" should "Generalize tail to the function drop" in {
    val l = List(1, 2, 3)
    List.drop(2, l) should be(List(3))
  }

  "Exercise 3.5" should "Implement dropWhile" in {
    val l: List[Int] = List(1, 2, 3, 4, 5, 5)
    val pred: (Int) => Boolean = (i: Int) => i <= 3
    List.dropWhile(l, pred) should be(List(4, 5, 5))
  }

  "Exercise 3.6" should "all but the last element of a List " in {
    val l = List(1, 2, 3)
    List.init(l) should be(List(1, 2))
  }

  "Exercise 3.7" should "short circuit" in {
    val l = List(0.0, 2.0, 3.0)
    List.product2(l) should be(0.0)
  }

  /**
   * See what happens when you pass Nil and Cons themselves to foldRight , like this:
   * foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) . 10 What do you think this
   * says about the relationship between foldRight and the data constructors of List ?
   */
  "Exercise 3.8" should "you pass Nil and Cons themselves to foldRight" in {
    val l = List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
    println(l)
  }

  "Exercise 3.9" should "define length" in {
    val l = List(1, 2, 3)
    List.length(l) should be(3)
  }

  "Exercise 3.10" should "define tail recursive foldLeft" in {
    val l = List(1, 2, 3)
    List.foldLeft(l, 0)(_ + _) should be(6)
  }

  "Exercise 3.11" should "define functions using foldLeft" in {
    // Write sum , product , and a function to compute the length of a list using foldLeft.
    val l = List(1, 2, 3, 4, 5)

    val sum = List.foldLeft(l, 0)(_ + _)
    sum should be(15)

    val product = List.foldLeft(l, 1)(_ * _)
    product should be(120)

    val length = List.foldLeft(l, 0)((acc: Int, c: Int) => acc + 1)
    length should be(5)
  }

  "Exercise 3.12" should "returns the reverse of a list (given List(1,2,3) it returns\nList(3,2,1) )" in {
    // Write a function that returns the reverse of a list (given List(1,2,3) it returns
    // List(3,2,1) ). See if you can write it using a fold.
    val l = List(1, 2, 3)
    val r = List.foldLeft(l, Nil: List[Int])((acc, c) => Cons(c, acc))
    r should be(List(3, 2, 1))
  }

  "Exercise 3.13" should "foldLeft in terms of foldRight ? How about the other way around?" in {
    val l = List(1, 2, 3)
    val sum = List.foldRight(l, 0)(_ + _)
    val sum1 = List.foldRightViaFoldLeft(l, 0)(_ + _)
    sum should be(sum1)
  }

  "Exercise 3.14" should "Implement append in terms of either foldLeft or foldRight ." in {
    val l = List(1, 2, 3)
    val l1 = List(1, 2, 3)
    val a = List.append(l, l1)
    a should be(List(1, 2, 3, 1, 2, 3))
  }

  "Exercise 3.15" should "implement flatMap" in {
    val l = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    List.flatMap(l) should be(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  "Exercise 3.16" should "implement map" in {
    //Write a function that transforms a list of integers by adding 1 to each element.
    //(Reminder: this should be a pure function that returns a new List !)
    val l = List(1, 2, 3)
    List.mapPlusOne(l) should be (List(2,3,4))
  }

  "Exercise 3.17" should "map double to string" in {
    val l = List(1.1, 2.2, 3.3)
    List.mapDoubleToString(l) should be (List("1.1", "2.2", "3.3"))
  }

  "Exercise 3.18" should "implement map" in {
    val l = List(1.1, 2.2, 3.3)
    List.map(l)(_.toString) should be (List("1.1", "2.2", "3.3"))
  }

  "Exercise 3.19" should "implement filter" in {
    val l = List(1, 2, 3)
    List.filter(l)(_>2) should be (List(3))
  }
}
