import scala.annotation.tailrec

object Lists {

  @tailrec
  def last(l: List[Int]): Option[Int] = l match {
    case Nil => None
    case x :: Nil => Some(x)
    case x :: xs => last(xs)
  }

  @tailrec
  def penultimate(l: List[Int]): Option[Int] = l match {
    case Nil => None
    case _ :: Nil => None
    case x :: _ :: Nil => Some(x)
    case x :: xs => penultimate(xs)
  }

}
