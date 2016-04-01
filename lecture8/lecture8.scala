trait Monoid[A]{
  def op(a1:A, a2:A) : A
  def zero : A
}

object Monoid {
  val intMultiplication = new Monoid[Int] {
    def op(a1:Int, a2:Int) = a1 * a2
    def zero : Int = 1
  }

  val intAddition = new Monoid[Int] {
    def op(a1:Int, a2:Int) = a1 + a2
    def zero : Int = 0
  }

  val stringConcat = new Monoid[String] {
    def op(a1:String, a2:String) = a1 + a2
    def zero : String = ""
  }
}

trait Foldable[F[_]] {
  def foldRight[A,B] (as: F[A]) (z: B) (f: (A,B) => B): B
  def foldLeft[A,B] (as: F[A]) (z: B) (f: (B,A) => B): B
  def flatMap[A,B] (as: F[A]) (f: A=>B) (mb: Monoid[B]): B
  def concatenate[A] (as: F[A]) (m: Monoid[A]): A = foldLeft (as) (m.zero) (m.op)
  def toList[A] (fa: F[A]) : List[A] = foldRight (fa) (Nil) ((e,acc) => e::acc)
}

//object Functors {
//  def map[A,B] (ga: Gen[A]) (f: A=>B): Gen[B]
//  def map[A,B] (la: List[A]) (f: A=>B): List[B]
//  def map[A,B] (oa: Option[A]) (f: A=>B): Option[B]
//}

trait Functor[F[_]] {
  def map[A,B] (fa: F[A]) (f: A=>B): F[B]
  def distribute[A,B] (fab: F[A,B]): (F[A],F[B]) =
    (map (fab) ((a,b) => a), map (fab) ((a,b) => b))
}

trait Monads[F[_]] {
  def unit[A] (a: => A): F[A]
  def flatMap[A,B] (fa: F[A]) (f: A=>F[B]): F[B]
  def map[A,B] (fa: F[A]) (f: A=>B): F[B] = flatMap (fa) (a => unit(f(a)))
  def map2[A,B,C] (fa: F[A]) (fb: F[B]) (f: (A,B)=>C): Option[C] = flatMap (fa) (a => map (fb) (b => f(a,b)))
  //def sequence[A] (lfa: List[F[A]]): F[List[A]] =
}

object Monads {
  def map2[A,B,C] (fa: Option[A]) (fb: Option[B]) (f: (A,B)=>C): Option[C] =
    flatMap (fa) (a => map (fb) (b => f(a,b)))
    // for(a <- fa; b <- fb) yield f(a,b)
}
