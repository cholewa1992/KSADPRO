/**
 * Advanced Programming.  Fake exam 2016.  This set of exam questions have been
 * prepared as a demonstration before the first written exam has been used in
 * ADPRO.
 */


/**
 * The questions will be greaded  manually.
 *
 * I do not recommend solving questions in an IDE to the point when they compile
 * and work (at least not until you have drafted all the answers).
 *
 * We will  be permissive  on small  syntactic issues  (semicolons, punctuation,
 * small deviations in  function names, switching between  curried and uncurried
 * parameters, unless  the question is about  currying, etc).
 *
 * We  will also  not check  whether the  type inference  succeeded (if  a human
 * reader could infer types).
 *
 * If you insert your answers in a  separate document remember to write the task
 * number before each answer, and answer questions  in the same order as in this
 * document.
 *
 * You can use any functions from the course (textbook, exercises) in your
 * solutions.
 *
 * You can use the provided build.sbt file to compile the exam. The same file
 * will work for the exam (but you might need to include some fpinscala files in
 * src/ if you use other functions in your solutions).  In general compiling
 * during a 4 hour exam is not recommended, as dependency problems can take all
 * your time.  But you are welcomed to do it, if you already solved all the
 * questions without compiling.
 **/

package adpro.examFake

object Q1 {

  import scala.collection.mutable.ListBuffer

  /**
   * Task 1.
   *
   * Translate the following scala function to a referentially
   * transparent version. You don't need to know what is a differential to
   * solve this task.
   */

  def listDifferentialImp (inList: ListBuffer[Int]): ListBuffer[Int] = {

    var result = ListBuffer[Int]()
    if (inList.size > 1) {
      var prev = inList.head
      for (curr <- inList.tail) {
        result += curr - prev // '+=' adds the right hand side as the last element in the list buffer
        prev = curr
      }
    }
    return result
  }

  def listDifferentialFun (inList: List[Int]): List[Int] = inList match {
    case prev::curr::xs => (curr - prev) :: listDifferentialFun (curr::xs)
    case _ => List()
  }

}



object Q2 {

  /**
   * Task 2.
   *
   * Implement  function onList  that  converts any  function of  type
   * String=>String to a function  of type List[Char]=>List[Char] that
   * satisies the following property:
   *
   * For any String s, and any function f:String => String:
   *
   *  f(s) == ( (onList (f)) (s.toList) ).mkString
   *
   * where mkString is  a function that converts (implodes)  a list of
   * characters back to a String.
   */

   def onList (f: String => String): List[Char] => List[Char] =
     list => f (list.mkString).toList

}



object Q3 {

  import fpinscala.monoids.Monoid
  import scala.language.higherKinds

  /**
   * Task 3.
   *
   * Implement a function foldBack that  folds an operator of a monoid
   * M, by traversing  through the list twice. If the  operator is "+"
   * and  the List  is  : List(x1,x2,x3),  then  your function  should
   * compute:
   *
   * (((((((z + x1) + x2) +x3) + x3) + x2) + x1) + z)
   *
   * where z = M.zero and + is M.op .
   */

  def foldBack[A] (l :List[A]) (implicit M: Monoid[A]): A =
    (l ::: l.reverse ::: List(M.zero)).foldLeft (M.zero) (M.op(_,_))
    //(l ::: l.reverse).foldLeft (M.zero) (M.op(_,_))
    
    /*
     * The below (uncommented line) gives the saem result, but does not
     * expand to the computation shown above (in line 109). 
     */

}



object Q4 {

  /**
   * Task 4.
   *
   * (One does not need to know probability theory to solve this task).
   *
   * Let  trait Event  be a  trait representing  random events  (as in
   * probability theory)  and P  be a probability  function, assigning
   * a  value  in  the  interval  [0;1] to  each  event  (an  instance
   * of  Event). Assume  the  declarations  below. The body  of  P  is
   * irrelevant.
   */

  trait Event
  trait Probability
  def P (e :Event) :Probability = ??? // assume that this is defined

  /**
   * The   function   conditionalP(E1,E2)    assigns   a   conditional
   * probability value  to a pair  of random  events E1 and  E2.  This
   * function  is sometimes  undefined.  Write  the type  signature of
   * conditionalP below.
   *
   * Note that  we are not asking  for a definition of  this function,
   * just for a type declaration.
   */

   def conditionalP (e1: Event, e2: Event): Option[Probability] = ???

}



object Q5 {

  /**
   * Task 5.
   *
   * Consider a type of lazy binary trees:
   */

  trait Tree[+A]
  case class Branch[+A] (l:() => Tree[A], r:() => Tree[A]) extends Tree[A]
  case object Leaf extends Tree[Nothing]

  /**
   * Implement a  convenience constructor  'branch' that is  both lazy
   * but does not require using explicit delays like Branch.
   */

  def branch[A] (l : =>Tree[A], r: =>Tree[A]) :Tree[A] = {
    lazy val left = l
    lazy val right = r
    Branch[A] (() => left, () => right)
  }
}



object Q6 {

  import monocle.Optional
  import monocle.Lens

  /**
   * Task 6.
   *
   * Formalize a lense leftFT, that allows accessing and replacing the
   * leftmost element of a deque stored in a finger tree.
   *
   * Recall the basic types from our implementation:
   */

  trait FingerTree[+A] {
    def addL[B >:A] (b: B) :FingerTree[B] = ??? // assume that this is implemented
  }
  case class Empty () extends FingerTree[Nothing]

  sealed trait ViewL[+A]
  case class NilTree () extends ViewL[Nothing]
  case class ConsL[A] (hd: A, tl: FingerTree[A]) extends ViewL[A]

  def viewL[A] (t: FingerTree[A]): ViewL[A] = ??? // assume that this is defined

  /* Use the addL and viewL to create a lens that extracts and allows
   * to modify the left most element of a finger tree. Either use the Monocle
   * API or (if you are writing in free text) use the notation from the paper of
   * Foster et al.
   *
   * Include the type of the lens (partial/total), and the put and get function.
   */

  def leftFT[A] = Optional[FingerTree[A],A] (t => viewL(t) match {
      case NilTree() => None
      case ConsL(hd,tl) => Some(hd)
    }) (a => t => viewL(t) match {
      case NilTree() => Empty().addL(a)
      case ConsL(hd,tl) => tl.addL(a)
    })

  /*
   *  Task 7.
   *
   *  Explain in English (or in Danish) which parts of your solution
   *  need to be updated (and how) in order to create a lense that provides the
   *  anologous functionality for the right end of the deque
   */

  /*
   *  To create a rightFT lense a viewR and addR function is needed.
   *  Bascially the only modification needed is to use these instead of the left functions currently being used.
   */

}




object Question7 {

  /* Task 8.
   *
   * Consider the standard library function (from the List companion object).
   *
   * def fill[A](n: Int)(elem: =>A): List[A]
   *
   * What is the meaning of (=>) in the above signature, and why the designers
   * of Scala library have used this type operator there? Explain in English (or
   * Danish).
   */

  /*
   * Lets consider a variation of the function
   * def fill'[A](n: Int)(elem: A): List[A]
   *
   * If a call is made to fill' then elem will evaluated at the point of invokation.
   * If a call is made to fill, on the contarary, elem will not be evaulated at invokation,
   * but rather a the time of usage within the body of the function.
   *
   * Thus (=>) makes the parameter lazily evaluated. If fill did not use this operator, in the
   * case where n is zero, then elem would have been evaluated for nothing.
   *
   */


  import fpinscala.state.RNG
  import fpinscala.testing.Gen

  /**
   * Task 9.
   *
   * Implement a generator multiplesOf10 that generates
   * integer numbers that are divisible by 10.
   *
   * Assume an implementation of Gen[A] as in the text book.
   * Also assume existance of arbitraryInt (implemented)
   *
   * Provide an explicit type for multiplesOf10
   */

  val arbitraryInt: Gen[Int] = ??? // assume that this exists.

  val multiplesOf10 = arbitraryInt.map (_ * 10)



  /**
   * Task 10.
   *
   * Implement a generator multipleOf10UpTo(m) that generaters integer
   * numbers divisible by 10, but smaller than m.
   *
   * Provide an explicit type for multiplesOf10UpTo
   */

  def multiplesOf10UpTo (m:Int): Gen[Int] =
    Gen.choose(0,m/10).map(_*10)
}



object Question8 {

  import fpinscala.state.RNG
  import fpinscala.testing.Gen

  val arbitraryInt :Gen[Int] = ??? // assume that this exists.
  def listOfN[A] (n: Int, g: Gen[A]) : Gen[List[A]] = ??? // assume that this exists.

  /**
   * Task 11.
   *
   * Below you will find two expressions that (apparently) generate randomly
   * sized integer lists.
   *
   * Write explicit types for v2 and v1.
   *
   * Explain in English (or Danish) what are the types of values v1 and v2 and
   * explain the difference between the computations that produce them.  The
   * explanation should not be long (4-5 lines will suffice).
   **/

  val v1: Gen[List[Int]] = arbitraryInt.flatMap (n => listOfN(n, arbitraryInt))
  val v2: (List[Int],RNG) = arbitraryInt.flatMap (n => listOfN(n, arbitraryInt)).
           sample.run (RNG.Simple(42))

  /*
   * v1's type is Gen[List[Int]]. The arbitraty ints are mapped to list of ints using the listOfN function
   * v2's type is (List[Int],RNG). This is because Gen contains a sample of type State[RNG,A]. State has a
   * function run of type RNG => (a,RNG). Thus when the sample is ran with a RNG (RNG.Simple(42)), a pair
   * of a and RNG is returned.
   */

}
