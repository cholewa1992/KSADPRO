sealed trait Either[+E, +A]
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object EitherExtension {
  def map[A,B,C,D] (ab: Either[A,B]) (f:A=>C) (g:B=>D): Either[C,D] = ab match {
    case Left(v) => Left(f(v)) 
    case Right(v) => Right(g(v))

  }
}


sealed trait Stream[+A]{
  def headOption[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A] (h: ()=>A, t: ()=>Stream[A]) extends Stream[A]

object StreamExtension{
  def cons[A] (h: => A, t: => Stream[A]) :Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail) 
  }

  def infinityOf (i: Int) : Stream[Int] = cons(i,infinityOf (i)) 
  

}
