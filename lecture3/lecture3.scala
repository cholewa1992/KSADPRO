sealed trait Stack[+A]
case object Empty extends Stack[Nothing]
case class Push[+A](top: A, below: Stack[A]) extends Stack[A]

//sealed trait Binary
//case object Empty extends Binary
//case class O(t: Binary) extends Binary
//case class I(t: Binary) extends Binary

sealed trait Option[+A]
{

  def map[B] (f: A => B) : Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def flatMap[B] (f: A => Option[B]) : Option[B] = this match {
    case None => None
    case Some(x) => f(x)
  }

  def filter (f: A => Boolean) : Option[A] = this match {
    case Some(x) => if(f(x)) Some(x) else None
    case None => None
  }


  def getOrElse[B >: A] (default: => B) : B = this match {
    case Some(x) => x
    case None => default
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]




object MyObject {
    def mean (xs: Seq[Double]) : Double =
      if (xs.length <= 0)
        throw new Exception()
      else
        xs.sum / xs.length

  def mean2 (xs: Seq[Double]) : Option[Double] =
    if(xs.length <= 0) None
    else Some(xs.sum / xs.length)
}



