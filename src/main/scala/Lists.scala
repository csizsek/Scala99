import scala.annotation.tailrec

object Lists {

  // P-01
  @tailrec
  def last[T](l: List[T]): Option[T] = l match {
    case Nil => None
    case x :: Nil => Some(x)
    case x :: xs => last(xs)
  }

  // P-02
  @tailrec
  def penultimate[T](l: List[T]): Option[T] = l match {
    case Nil => None
    case _ :: Nil => None
    case x :: _ :: Nil => Some(x)
    case x :: xs => penultimate(xs)
  }

  // P-03
  @tailrec
  def nth[T](n: Int, l: List[T]): Option[T] = (n, l) match {
    case (_, Nil) => None
    case (0, x :: xs) => Some(x)
    case (k, x :: xs) =>
      if (k < 0) None
      else nth(k - 1, xs)
  }

  // P-04
  @tailrec
  def length[T](l: List[T], n: Int = 0): Int = l match {
    case Nil => n
    case x :: xs => length(xs, n+1)
  }

  // P-05
  @tailrec
  def reverse[T](l: List[T], reversed: List[T] = List()): List[T] = l match {
    case Nil => reversed
    case x :: Nil => x :: reversed
    case x :: xs => reverse(xs, x :: reversed)
  }

  // P-06
  @tailrec
  def isPalindrome[T](l: List[T]): Boolean = l match {
    case Nil => true
    case x :: Nil => true
    case x :: xs =>
      if (x == xs.last) isPalindrome(xs.dropRight(1))
      else false
  }

  // P-07
  def flatten(l: Any): List[Any] = l match {
    case Nil => List()
    case x :: Nil => flatten(x)
    case x :: xs => flatten(x) ++ flatten(xs)
    case _ => List(l)
  }

  // P-08
  def compress(l: List[Any]): List[Any] = l match {
    case x :: y :: xs => if (x == y) compress(y :: xs) else x :: compress(y :: xs)
    case _ => l
  }

  // P-09
  def pack(l: List[Any]): List[Any] = {

    @tailrec
    def packHelper(c: List[List[Any]], l: List[Any]): List[List[Any]] = (c, l) match {
      case (cs, Nil) => cs
      case (cs, x :: xs) =>
        if (cs.last.last == x) packHelper(cs.dropRight(1) :+ (cs.last :+ x), xs)
        else packHelper(cs :+ List(x), xs)
    }

    packHelper(List(List(l.head)), l.tail)
  }

  // P-10
  def encode(l: List[Any]): List[(Int, Any)] = {

    @tailrec
    def encodeHelper(c: List[(Int, Any)], l: List[Any]): List[(Int, Any)] = (c, l) match {
      case (cs, Nil) => cs
      case (cs, x :: xs) =>
        if (cs.last._2 == x) encodeHelper(cs.dropRight(1) :+(cs.last._1 + 1, x), xs)
        else encodeHelper(cs :+(1, x), xs)
    }

    encodeHelper(List((1, l.head)), l.tail)
  }

  // P-11
  def encodeModified[T](l: List[T]): List[Any] = {

    @tailrec
    def encodeModifiedHelper(c: List[Any], l: List[T]): List[Any] = (c, l) match {
      case (cs, Nil) => cs
      case (cs, x :: xs) => cs.last match {
        case (d:Int, y) =>
          if (y == x) encodeModifiedHelper(cs.dropRight(1) :+ (d + 1, x), xs)
          else encodeModifiedHelper(cs :+ x, xs)
        case y =>
          if (y == x) encodeModifiedHelper(cs.dropRight(1) :+ (2, x), xs)
          else encodeModifiedHelper(cs :+ x, xs)
      }
    }

    encodeModifiedHelper(List((1, l.head)), l.tail)
  }

  // P-12
  def decode[T](l: List[(Int, T)]): List[T] = l match {
    case Nil => Nil
    case (0, _) :: xs => decode(xs)
    case (c, x) :: xs => x :: decode((c - 1, x) :: xs)
  }

  // P-14
  def duplicate[T](l: List[T]): List[T] = l match {
    case Nil => Nil
    case x :: xs => x :: x :: duplicate(xs)
  }

  // P-15
  def duplicateN[T](n: Int, l: List[T]): List[T] = {

    def multiples(k: Int, t: T): List[T] = k match {
      case 0 => Nil
      case m => t :: multiples(m-1, t)
    }

    l match {
      case Nil => Nil
      case x :: xs => multiples(n, x) ++ duplicateN(n, xs)
    }
  }

  // P-16
  def drop[T](n: Int, l: List[T], k: Int = 1): List[T] = l match {
    case Nil => Nil
    case x :: xs =>
      if (k == n) drop(n, xs, 1)
      else x :: drop(n, xs, k+1)
  }

  // P-17
  @tailrec
  def split[T](n: Int, l: List[T], k: List[T] = List()): (List[T], List[T]) = (n, l, k) match {
    case (0, _, _) => (k, l)
    case (_, Nil, _) => (k, l)
    case (i, x, y) => split(i-1, x.tail, y :+ x.head)
  }

  // P-18
  def slice[T](s: Int, e: Int, l: List[T]): List[T] = ???

}


