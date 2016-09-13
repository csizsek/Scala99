object Lists {

  def last(a: List[Int]): Option[Int] = {
    a match {
      case Nil => None
      case x :: Nil => Some(x)
      case x :: xs => last(xs)
    }
  }

  def penultimate(a: List[Int]): Option[Int] = {
    a match {
      case Nil => None
      case x :: Nil => None
      case y :: x :: Nil => Some(y)
      case y :: x :: xs => penultimate(x :: xs)
    }
  }

}
