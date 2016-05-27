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

  //Task 1.
  def checksumFun (in :String): Int = {
    @annotation.tailrec
    def checksumRec (in: List[Char]) (res: Int): Int = in match {
      case x::xs => checksumRec(xs) ((res + x.toInt) % 0xffff)
      case Nil => res
    }
    checksumRec (in.toList) (0)
  }

  //Task 2.

  /*
   * The outer function checksumFun is not tail recursive because it does not
   * make any recursive calls. The inner function checksumRec is tail recursive
   * as the recursive calls on line 17 is in tail position
   */

}


object Q2 {
  import fpinscala.monads.Functor
  import scala.language.higherKinds

  //Task 3.
  def onList[A] (f: A => A): List[A] => List[A] = list => list.map(f)

  //Task 4.
  def onCollection[C[_],A] (f: A => A)
  (implicit functorC: Functor[C]): C[A] => C[A] = functorC.map (_) (f)

}

object Q3 {

  import fpinscala.monoids.Monoid
  import scala.language.higherKinds

  //Task 5.
  def foldBack[A] (l: List[A]) (implicit M: Monoid[A]): A =
    (l ::: l.reverse).foldLeft (M.zero) (M.op)

}

object Q4 {

  type Computation[A] = A => Either[A,String]

  //Task 6.
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


  //Task 7.
  def multiply (t: Tree[Int]): Int = t match {

    case Branch(l,a,r) => {
      if(a != 0){
        //Getting left branch
        val lv = multiply(l())
        if(lv != 0) {
          //Getting right branch
          val rv = multiply(r())
          if(rv != 0) {
            lv * a * rv
          } else 0
        } else 0
      } else 0
    }
    case Leaf(a) => a

  }

  // Task 8. (answer below in a comment)
  /*
   * The only way the result of multiply can be zero is if one of the elements
   * is zero. Therefore, as soon as a zero is meet, the function will terminate
   * and return 0. If it does not meet zero, it will continue the traversal.
   */

  def exception[A]: Tree[A] = throw new RuntimeException()
  def testTree1: Tree[Int] = Branch(() => exception, 0, () => exception)
  def testTree2: Tree[Int] = Branch(() => Leaf(0), 1, () => exception)

  /*
   * The function can be tested if we throw runtime exceptions if it is too eag-
   * er. If multiply is implemented correctly (and assuming that it traverses
   * left branch before right, then the two above test trees will not fail beca-
   * use multiply is not more eager than needed.
   *
   * Property testing on arbitrary finite trees can be used to test that the re-
   * ult will be correct while testing on arbitrary infinite trees containing at
   * least one element being zero will show that the function will terminate co-
   * rrectly on such trees.
   *
   * Testing that the function will never terminate on infinite trees that does
   * not contain a zero cannot be tested, but can be formally proven.
   */

}


object Q6 {

  sealed trait Nat[+A]
  case object Zero extends Nat[Unit]
  case class Succ[A] (pred: A) extends Nat[A]

  def zero: Nat[Unit] = Zero                  // Task 9.
  val one: Nat[Nat[Unit]] = Succ (zero)       // Task 9.
  val two: Nat[Nat[Nat[Unit]]] = Succ (one)   // Task 9.


  //Task 10.
  def plus2[A] (x : Nat[A]): Nat[Nat[Nat[A]]] = Succ(Succ(x))

}
