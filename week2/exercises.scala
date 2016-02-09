// Advanced Programming, Exercises by A. WÄ…sowski, IT University of Copenhagen
//
// AUTHOR1: Christoffer Bjerre (chbj@itu.dk)
// AUTHOR2: Jacob B. Cholewa (jbec@itu.dk)
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
// To run the compiled file do "scala Exercises"
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


//The result of exercise 1 is x = 3 

// An ADT of Lists

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  // override function application to provide a factory of lists (convenience)

  def apply[A](as: A*): List[A] = // Variadic function
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

    // Exercise 2

    def tail[A] (as: List[A]) :List[A] = 
      as match{
        case Cons(x,xs) => xs
        case Nil => Nil
      }

    // Exercise 3

    def setHead[A] (as: List[A], newHead: A) : List[A] = 
      as match {
        case Cons(x,xs) => Cons(newHead,xs)
        case Nil => Nil
      }

    // Exercise 4

    def drop[A] (l: List[A], n: Int) : List[A] = 
      l match {
        case Cons(x,xs) if n > 0 => drop (xs,n-1)
        case _ => l
      }

    // Exercise 5

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
      l match {
        case Cons(x,xs) if f (x) => dropWhile (xs,f)
        case _ => l
      }

    // Exercise 6

    def init[A](l: List[A]): List[A] = 
      l match {
        case Cons(x,Nil) => Nil
        case Cons(x,xs) => Cons(x, init (xs))
        case _ => Nil
      }

    // Exercise 7 is in the bottom of the file

    // Exercise 8

    def foldRight[A,B] (as :List[A], z: B) (f : (A,B)=> B) :B = 
      as match {
        case Nil => z
        case Cons (x,xs) => f (x, foldRight (xs,z) (f))
      }

    def length[A] (as: List[A]): Int = foldRight (as,0) ((x,y) => 1 + y)

    // Exercise 9

    def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B) : B = 
      as match {
        case Nil => z 
        case Cons(x,xs) => foldLeft (xs,f (z,x)) (f)
      }

    // Exercise 10

    def sum (as : List[Int]) : Int = foldLeft (as,0) (_ + _)
    def product (as :List[Int]) : Int = foldLeft (as,1) (_ * _)
    def length1 (as :List[Int]) : Int = foldLeft (as,0) ((x,y) => 1 + x)

    // Exercise 11

    def reverse[A] (as :List[A]) :List[A] = foldLeft (as,(Nil:List[A])) ((x,y) => Cons(y,x))

    // Exercise 12

    def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B) : B = 
      foldLeft (reverse (as), z) ((a,b) => f (b,a))


    //def foldLeft1[A,B] (as: List[A], z: B) (f: (B,A) => B) : B =
    // I don't understand what I'm being asked to do.

    // Exercise 13

    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

    def concat[A] (as: List[List[A]]) :List[A] = as match {
      case Nil => Nil 
      case Cons(x,xs) => append(x,concat (xs))
    }

    // Exercise 14

    // I don't know what map version that is being refered too.

    def map[A,B] (a :List[A]) (f :A => B) :List[B] = 
      foldRight1 (a,(Nil:List[B])) ((x,y) => Cons(f(x),y))


    // Exercise 15 (no coding)
    
    //Because the resulting mapped list would be backwards.

    // Exercise 16

    def filter[A] (as: List[A]) (f: A => Boolean) : List[A] = as match {
      case Cons(x,xs) =>  if (f (x)) Cons(x,filter (xs) (f)) 
                          else filter (xs) (f)
      case _ => Nil
    }


    // Exercise 17

    def flatMap[A,B](as: List[A])(f: A => List[B]) : List[B] = 
      foldRight1 (as,(Nil:List[B])) ((x,y) => append(f(x),y))

    // Exercise 18

    def filter1[A] (l: List[A]) (p: A => Boolean) :List[A] = 
      flatMap (l) (x => if (p(x)) Cons(x,Nil) else Nil)

    // Exercise 19

    def add (l: List[Int]) (r: List[Int]): List[Int] = (l,r) match {
      case (Cons(x,xs),Cons(y,ys)) => Cons((x+y),add (xs) (ys))
      case _ => Nil
    }

    // Exercise 20

    def zipWith[A,B,C] (f : (A,B)=>C) (l: List[A], r: List[B]) : List[C] = (l,r) match {
      case (Cons(x,xs),Cons(y,ys)) => Cons(f(x,y), zipWith (f) (xs,ys))
      case _ => Nil
    }

    // Exercise 21

    def hasSubsequence[A] (sup: List[A], sub: List[A]) :Boolean = sup match {
      case Cons(x,xs) => 
        if((zipWith[A,A,A] ((x,y) => x) (sup,sub)) == sub) true 
        else hasSubsequence (xs,sub)
      case _ => false
    }

    // Exercise 22

    def pascal (n :Int) : List[Int] = n match {
      case 1 => List(1)
      case n => 
        val l = pascal(n-1)
        val z = zipWith[Int,Int,Int] (_ + _) (l, tail (l))
        append (Cons(1,z), List(1))
    } 

    // a test: pascal (4) = Cons(1,Cons(3,Cons(3,Cons(1,Nil))))
}

// Exercise 7

object Exercise7 {

  case class SalaryLine(name: String, amount: Integer)

  def maximumSalary (salaries: List[SalaryLine]) :Integer = {
    def ms (s: List[SalaryLine]) (max:Int):Integer =
      s match {
        case Cons(SalaryLine(x,y),xs) if y > max => ms (xs) (y)
        case Cons(SalaryLine(x,y),xs) => ms (xs) (max)
        case _ => max 
      }
    ms (salaries) (0)
  }

  val test_case = List( SalaryLine("John",41),
    SalaryLine("Alice", 42),
    SalaryLine("Bob",40))
}
