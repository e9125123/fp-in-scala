/**
 * Code to go with chapter 4 of 'Functional Programming in Scala'
 */
object Chapter4 {

  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(a) => f(a)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case _ => this
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case None => None
      case Some(a) => {
        if (f(a)) this else None
      }
    }
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance1(xs: Seq[Double]): Option[Double] = xs match {
    case Seq() => None
    case _ => {
      val m = xs.sum / xs.length
      val ms = xs.map((d: Double) => Math.pow(d - m, 2))
      Some(ms.sum / ms.size)
    }
  }

  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (aa => b map (bb => f(aa, bb)))
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  def sequence_2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)((aa: Option[A]) => aa)

  sealed trait Either[+E, +A] {

    def map[B](f: A => B): Either[E, B] = this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = ???

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = ???
  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

}
