import scala.annotation.tailrec

object Lists {

  // P-01
  @tailrec
  def last(l: List[Any]): Option[Any] = l match {
    case Nil => None
    case x :: Nil => Some(x)
    case x :: xs => last(xs)
  }

  // P-02
  @tailrec
  def penultimate(l: List[Any]): Option[Any] = l match {
    case Nil => None
    case _ :: Nil => None
    case x :: _ :: Nil => Some(x)
    case x :: xs => penultimate(xs)
  }

  // P-03
  @tailrec
  def nth(n: Int, l: List[Any]): Option[Any] = (n, l) match {
    case (_, Nil) => None
    case (0, x :: xs) => Some(x)
    case (k, x :: xs) =>
      if (k < 0) None
      else nth(k - 1, xs)
  }

  // P-04
  def length(l: List[Any]): Int = {
    def len(n: Int, l: List[Any]): Int = l match {
      case Nil => 0
      case x :: xs => len(n+1, xs) + 1
    }
    len(0, l)
  }

  // P-05
  def reverse(l: List[Any]): List[Any] = l match {
    case Nil => Nil
    case x :: xs => reverse(xs) :+ x
  }

  // P-06
  @tailrec
  def isPalindrome(l: List[Any]): Boolean = l match {
    case Nil => true
    case x :: Nil => true
    case x :: xs =>
      if (x == xs.last) isPalindrome(xs.dropRight(1))
      else false
  }

  // P-07
  def flatten(l: Any): List[Any] = {
    l match {
      case Nil => List()
      case x :: Nil => flatten(x)
      case x :: xs => flatten(x) ++ flatten(xs)
      case _ => List(l)
    }
  }

  // P-08
  def compress(l: List[Any]): List[Any] = l match {
    case x :: y :: xs => if (x == y) compress(y :: xs) else x :: compress(y :: xs)
    case _ => l
  }

  // P-09
  def pack(l: List[Any]): List[Any] = {
    def packHelper(c: List[List[Any]], l: List[Any]) : List[List[Any]] = (c, l) match {
      case (cs, Nil) => cs
      case (cs, x :: xs) =>
        if (cs.last.last == x) packHelper(cs.dropRight(1) :+ (cs.last :+ x), xs)
        else packHelper(cs :+ List(x), xs)
    }
    packHelper(List(List(l.head)), l.tail)
  }

  // P-10
  def encode(l: List[Any]): List[(Int, Any)] = {
    def encodeHelper(c: List[(Int, Any)], l: List[Any]): List[(Int, Any)] = (c, l) match {
      case (cs, Nil) => cs
      case (cs, x :: xs) =>
        if (cs.last._2 == x) encodeHelper(cs.dropRight(1) :+ (cs.last._1 + 1, x), xs)
        else encodeHelper(cs :+ (1, x), xs)
    }
    encodeHelper(List((1, l.head)), l.tail)
  }

}
