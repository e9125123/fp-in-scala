/**
 * Code to go with chapter 4 of 'Functional Programming in Scala'
 */
object Chapter4 {

  sealed trait Option[+A]
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

}
