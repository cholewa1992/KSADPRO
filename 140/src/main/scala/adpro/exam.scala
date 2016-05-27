// Your name: Jacob Benjamin Cholewa
// Your ITU email: jbec@itu.dk

package adpro.exam2016
object Q1 {
  def checksumImp (in: String) :Int = {
    var result = 0
    for (c <- in.toList)
      result = (result + c.toInt) % 0xffff
      return result
  }

  def checksumFun (in :String): Int = {
    @annotation.tailrec
    def checksumRec (in: List[Char]) (res: Int): Int = in match {
      case x::xs => checksumRec(xs) ((res + x.toInt) % 0xffff)
      case Nil => res
    }
    checksumRec (in.toList) (0)
  }

  // Write your answer for Task 2 here.

  /*
   * The outer methode checksumFun is not tail recursive because it does not ma-
   * ke any recursive calls, the inner function checksunRec, is however, tail
   * recursive as the recursive calls on line 16 is in tail position
   *
   */


}


object Q2 {
  import fpinscala.monads.Functor
  import scala.language.higherKinds

  def onList[A] (f: A => A): List[A] => List[A] = list => list.map(f)

  def onCollection[C[_],A] (f: A => A)
  (implicit functorC: Functor[C]): C[A] => C[A] = functorC.map (_) (f)

}

object Q3 {

  import fpinscala.monoids.Monoid
  import scala.language.higherKinds

  def foldBack[A] (l: List[A]) (implicit M: Monoid[A]): A =
    (l ::: l.reverse).foldLeft (M.zero) (M.op)

}

object Q4 {

  type Computation[A] = A => Either[A,String]

  def run[A] (init: A) (progs: List[Computation[A]]): (A,List[String]) =
    progs match {
      case x::xs => x(init) match {
        case Left(a) => run(a)(xs)
        case Right(m) =>
          val (a,l) = run(init)(xs)
          (a,m::l)
      }
      case Nil => (init,Nil)
    }

}


object Q5 {

  sealed trait Tree[A]
  case class Branch[A] (l: () => Tree[A], a: A, r:() => Tree[A]) extends Tree[A]
  case class Leaf[A] (a: A) extends Tree[A]

  def multiply (t: Tree[Int]): Int = t match {

    case Branch(l,a,r) if a != 0 => {
      //Getting left branch
      val lv = multiply(l())
      if(lv != 0) {

        //Getting right branch
        val rv = multiply(r())
        if(rv == 0) {

          lv * a * rv
        } else 0
      } else 0
    }
    case Leaf(a) => a
    case _ => 0

  }

  // Task 8. (answer below in a comment)
  /*
   * The only was the result of multiply can be zero if is one of the a elements
   * was zero. Therefore, as soon as a zero is meet, the function will terminate 
   * and return 0
   */
  

  def exception[A]: Tree[A] = throw new RuntimeException()
  def testTree1: Tree[Int] = Branch(() => exception[Int], 0, () => exception[Int])
  def testTree2: Tree[Int] = Branch(() => Leaf(0), 1, () => exception[Int])


  /*
   * If multiply is implemented correctly (and assuming that it traverses left  
   * breanch before right, then the two above test trees will not fail because
   * multiply is not more earger that needed.
   */

}


object Q6 {

  sealed trait Nat[+A]
  case object Zero extends Nat[Unit]
  case class Succ[A] (pred: A) extends Nat[A]

  val zero /* : ... */ = Zero            // Task 9.
  val one  /* : ... */ = Succ (zero)     // Task 9.
  val two  /* : ... */ = Succ (one)      // Task 9.


  // def plus2  (x : ... ) : ... = ???        // Task 10.

}
