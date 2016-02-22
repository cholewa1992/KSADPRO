// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// AUTHOR1: Christoffer Bjerre (chbj@itu.dk)
// AUTHOR2: Jacob B. Cholewa (jbec@itu.dk)
// Group number: 46
//
// Write names and ITU email addresses of both group members that contributed to
// the solution of the exercise (in alphabetical order by family name).
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file is meant to be compiled as follows:
//
// scalac Exercises.scala
//
// or
//
// fsc Exercises.scala
//
// To run the compiled file do "scala Tests"
//
// To load the file int the REPL start the 'scala' interpreter and issue the
// command ':load Exercises.scala'. Now you can interactively experiment with
// your code.
//
// Continue solving exercises in the order presented in the PDF file. Large
// parts of the file are commented in order to make sure that the exercise
// compiles.  Uncomment and complete fragments as you proceed.  The file shall
// always compile and run after you are done with each exercise (if you do them
// in order).  Please compile and test frequently.

// Exercise  1

/* I created OrderedPoint as a trait instead of a class, so I can mix it into
 * Points (this allows me to use java.awt.Point constructors without
 * reimplementing them). As constructors are not inherited, I would have to
 * reimplement them in my subclass.  This is not a problem if I mix in a trait
 * construction time. */

sealed trait OrderedPoint extends java.awt.Point with scala.math.Ordered[java.awt.Point] {
  override def compare (that :java.awt.Point) :Int =
    if(this.x == that.x && this.y == that.y) 0
    else if(this.x < that.x || (this.x == that.x && this.y < that.y)) -1
    else 1
}

// Chapter 3

sealed trait Tree[+A]
case class Leaf[A] (value: A) extends Tree[A]
case class Branch[A] (left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 2 (3.25)

  def size[A] (t :Tree[A]) :Int = t match {
    case Leaf(v) => 1
    case Branch(r,l) => (size (r)) + (size (l)) + 1
  }

  // Exercise 3 (3.26)

  def maximum (t: Tree[Int]) :Int = t match {
    case Leaf(v) => v
    case Branch(r,l) => (maximum (r)) max (maximum (l)) 
  }

  // Exercise 4 (3.27)

  def depth[A] (t :Tree[A]) :Int = t match {
    case Leaf(v) => 1
    case Branch(r,l) => (depth (r)) max (depth (l)) + 1
  }

  // Exercise 5 (3.28)

  def map[A,B] (t: Tree[A]) (f: A => B) : Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(r,l) => Branch((map (r)(f)), (map (l)(f)))
  }

  // Exercise 6 (3.29)

  def fold[A,B] (t: Tree[A]) (f: (B,B) => B) (g: A => B) :B = t match {
    case Leaf(v) => g(v)
    case Branch(r,l) => f ((fold (r) (f) (g)), (fold (l) (f) (g)))
  }

  def size1[A] (t: Tree[A]) :Int = fold[A,Int] (t) ((x,y) => x + y + 1) (x => 1)
  def maximum1 (t: Tree[Int]) :Int = fold[Int,Int] (t) ((x,y) => x max y) (x => x)
  def depth1[A] (t: Tree[A]) :Int = fold[A,Int] (t) ((x,y) => (x max y) + 1) (x => 0)
  def map1[A,B] (t: Tree[A]) (f: A => B) :Tree[B] = fold[A,Tree[B]] (t) (Branch(_,_)) (x => Leaf(f(x))) 

}

sealed trait Option[+A] 
{
  def map[B] (f: A=>B) : Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def getOrElse[B >: A] (default: => B) :B = this match {
    case Some(x) => x
    case None => default
  }

  def flatMap[B] (f: A=>Option[B]) : Option[B] = this match {
    case None => None
    case Some(x) => f(x)
  }

  def orElse[B >: A] (ob : => Option[B]) : Option[B] = this match {
    case None => ob
    case Some(x) => Some(x)
  }
  def filter (f: A => Boolean) : Option[A] = this match {
    case Some(x) => if(f(x)) Some(x) else None
    case None => None
  }
}
    case class Some[+A] (get: A) extends Option[A] 
    case object None extends Option[Nothing] 

    object ExercisesOption {

      // Remember that mean is implemented in Chapter 4 of the text book

      def mean(xs: Seq[Double]): Option[Double] =
        if (xs.isEmpty) None
        else Some(xs.sum / xs.length)

        // Exercise 8 (4.2) I know that this is correct; I'am however getting an annoying type error that I can't figure out how to resolve.

        def variance (xs: Seq[Double]) : Option[Double] = { 
          val m : Option[Double] = mean(xs)
          //val u : Seq[Double] = xs.flatMap(x => m.map(y => math.pow(x-y,2)))
          val u : Seq[Double] = for(x <- xs; y <- m) yield math.pow(x-y,2)
          mean (u)
        }

        // Exercise 9 (4.3)

        def map2[A,B,C] (ao: Option[A], bo: Option[B]) (f: (A,B) => C) :Option[C] = for(a <- ao; b <- bo) yield f(a,b)

        // Exercise 10 (4.4)

        def sequence[A] (aos: List[Option[A]]) : Option[List[A]] = aos.foldRight[Option[List[A]]] (Some(List())) ((e,acc) => acc.flatMap(list => e.map(a => a::list)))
        // Exercise 11 (4.5)

        def traverse[A,B] (as: List[A]) (f :A => Option[B]) :Option[List[B]] = as match {
          case x::xs => (traverse (xs) (f)).flatMap(list => f(x).map(a => a::list))
          case Nil => Some(List())
        }

    }


    // Test cases for running in the compiled vesion (uncomment as you go, or paste
    // them into REPL in the interactive version)

    object Tests extends App {

      // Exercise 1
      val p = new java.awt.Point(0,1) with OrderedPoint
      val q = new java.awt.Point(0,2) with OrderedPoint
      assert(p < q)

      // Notice how we are using nice infix comparison on java.awt
      // objects that were implemented way before Scala existed :) (And without the
      // library implementing a suitable comparator). We did not have to recompile
      // java.awt.Point


      // Exercise 2
      assert (Tree.size (Branch(Leaf(1), Leaf(2))) == 3)

      // Exercise 3
      assert (Tree.maximum (Branch(Leaf(1), Leaf(2))) == 2)

      // Exercise 4
      val t4 = Branch(Leaf(1), Branch(Branch(Leaf(2),Leaf(3)),Leaf(4)))
      assert (Tree.depth (t4) == 3)

      // Exercise 5
      val t5 = Branch(Leaf("1"), Branch(Branch(Leaf("2"),Leaf("3")),Leaf("4")))
      assert (Tree.map (t4) (_.toString) == t5)

      // Exercise 6
      assert (Tree.size1 (Branch(Leaf(1), Leaf(2))) == 3)
      assert (Tree.maximum1 (Branch(Leaf(1), Leaf(2))) == 2)
      assert (Tree.depth1 (t4) == 3)
      assert (Tree.map1 (t4) (_.toString) == t5)

      // Exercise 7
      assert (Some(1).map (x => x +1) == Some(2))
      assert (Some(41).getOrElse(42) == 41)
      assert (None.getOrElse(42) == 42)
      assert (Some(1).flatMap (x => Some(x+1)) == Some(2))
      assert ((None: Option[Int]).flatMap[Int] (x => Some(x+1)) == None)
      assert (Some(41).orElse (Some(42)) == Some(41))
      assert (None.orElse (Some(42)) == Some(42))
      assert (Some(42).filter(_ == 42) == Some(42))
      assert (Some(41).filter(_ == 42) == None)
      assert ((None: Option[Int]).filter(_ == 42) == None)

      // Exercise 8
      //assert (ExercisesOption.variance (List(42,42,42)) == Some(0.0))
      //assert (ExercisesOption.variance (List()) == None)


      // Exercise 9
      assert (ExercisesOption.map2 (Some(42),Some(7)) (_ + _) == Some(49))
      assert (ExercisesOption.map2 (Some(42),None) (_ + _) == None)
      assert (ExercisesOption.map2 (None: Option[Int],Some(7)) (_ + _) == None)
      assert (ExercisesOption.map2 (None: Option[Int],None) (_ + _) == None)

      // Exercise 10
      assert (ExercisesOption.sequence (List(Some(1), Some(2), Some(42))) == Some(List(1,2,42)))
      assert (ExercisesOption.sequence (List(None,    Some(2), Some(42))) == None)
      assert (ExercisesOption.sequence (List(Some(1), None,    Some(42))) == None)
      assert (ExercisesOption.sequence (List(Some(1), Some(2), None    )) == None)

      // Exercise 11
      def f (n: Int) :Option[Int] = if (n%2 == 0) Some(n) else None
      assert (ExercisesOption.traverse (List(1,2,42)) (Some(_)) == Some(List(1,2,42)))
      assert (ExercisesOption.traverse (List(1,2,42)) (f) == None)

    }
