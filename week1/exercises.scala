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

// The extension of App allows writing statements at class top level (the so
// called default constructor). For App objects they will be executed as if they
// were placed in the main method in Java.

object Exercises extends App {

  // Exercise 3

  def power (x:Double, n:Int):Double = {
    if(n > 0){
      if(n % 2 == 0) (power (x,n/2)) * (power (x,n/2)) //Not tail recursive
      else x * (power (x, n-1)) //Tail recursive
    }
    else if(n == 0) 1.0
    else 1.0 / (power (x,-n)) //Tail recursive
  }

  //If n is very large, the stack depth will rapidly grow due to the exponential number of recursive calls. With the current logic, it is difficult to make the function tail recursive due to the fact that we have two new recursive calls for each call.


  // A few tests, uncomment when your implementation is ready.

  assert (power (2.0, 2) == 4.0)
  assert (power (1.0, 42) == 1.0)

  //
  // The above assertions should pass when you call "scala Exercises".
  //
  // The following one should fail. Uncomment to check that assert works as
  // expected:
  //
  // assert (power (1.0, 42) == 2.0)

  // add 2-3 more tests:

  assert (power (10.0, -1) == 0.1)
  assert (power (10.0,  0) == 1.0)
  assert (power (10.0,  3) == 1000.0)
  assert (power (10.0,  4) == 10000.0)

  // Exercise 4

  def fib (n:Int):Int = {
    @annotation.tailrec
    def f (i:Int) (a:Int) (b:Int) : Int = {
      if(i >= n) b
      else f (i+1) (a+b) (a) 
    }
    f (0) (0) (1)
  }

  // some tests (uncomment, add more):

  assert (fib (1) == 0)
  assert (fib (2) == 1)
  assert (fib (3) == 1)
  assert (fib (4) == 2)
  assert (fib (5) == 3)
  assert (fib (6) == 5)
  assert (fib (7) == 8)

  // Exercise 5

  // A simple object describing a cost line; implemented imperatively, Java
  // style (this way until we learn more Scala)
  class Expense {

    // A constructor definition
    def this (tag :String, price :Int) = {
      this()
      this.tag = tag
      this.price = price
    }

    var tag   :String = "" // a tag line in the accounting system
    var price :Int    = 0 // the price is in cents
  }

  // computes the total of expenses in cents

  def total (expenses: Array[Expense]) :Int = expenses.foldLeft(0)((r,c) => c.price + r) 


  val testcase1 = Array[Expense](
    new Expense("Coffee", 450),
    new Expense("Cake", 350) )

  val testcase2 = Array[Expense](new Expense("Milk", 10)) ++ testcase1

  assert (total (testcase1) == 800) // uncomment
  assert (total (testcase2) == 810)

  // Exercise 6

  def isSorted[A] (as: Array[A], ordered: (A,A) =>  Boolean) :Boolean = {
    @annotation.tailrec
    def loop(n:Int, prev:A):Boolean = {
      if(n >= as.length) return true
      else ordered(prev,as(n)) && loop(n+1,as(n))
    }
    loop(1,as(0))
  }

  // some tests (uncomment)

  assert ( isSorted (Array(1,2,3,4,5,6), (a: Int, b: Int)=> a <= b))
  assert (!isSorted (Array(6,2,3,4,5,6), (a: Int, b: Int)=> a <= b))
  assert (!isSorted (Array(1,2,3,4,5,1), (a: Int, b: Int)=> a <= b))

  // add two tests with another type, for example an Array[String]


  assert( isSorted (Array("a","b","c","d","e","f","g"), (a: String, b: String)=> a <= b))
  assert(!isSorted (Array("a","c","b","d","e","f","g"), (a: String, b: String)=> a <= b))

  // Exercise 7: a curried version of solution to exercise 3

  def power1 (x:Double) (n:Int):Double = {
    if(n > 0){
      if(n % 2 == 0) (power1 (x) (n/2)) * (power1 (x) (n/2)) //Not tail recursive
      else x * (power1 (x) (n-1)) //Tail recursive
    }
    else if(n == 0) 1.0
    else 1.0 / (power1 (x) (-n)) //Tail recursive
  }

  assert (power1 (2.0)  (2)   == 4.0)
  assert (power1 (1.0)  (42)  == 1.0)
  assert (power1 (10.0) (-1)  == 0.1)
  assert (power1 (10.0) (0)   == 1.0)
  assert (power1 (10.0) (3)   == 1000.0)
  assert (power1 (10.0) (4)   == 10000.0)

  // Exercise 8

  def curry[A,B,C] (f: (A,B)=>C) : A => (B => C) = ((a:A) => ((b:B) => f(a,b)))  

  assert (curry (power) (2.0)  (2)   == 4.0)
  assert (curry (power) (1.0)  (42)  == 1.0)
  assert (curry (power) (10.0) (-1)  == 0.1)
  assert (curry (power) (10.0) (0)   == 1.0)
  assert (curry (power) (10.0) (3)   == 1000.0)
  assert (curry (power) (10.0) (4)   == 10000.0)

  // Exercise 9

  def uncurry[A,B,C] (f: A => B => C) : (A,B) => C = (a,b) => f (a) (b)

  assert (uncurry (power1) (2.0,   2) == 4.0)
  assert (uncurry (power1) (1.0,  42) == 1.0)
  assert (uncurry (power1) (10.0, -1) == 0.1)
  assert (uncurry (power1) (10.0,  0) == 1.0)
  assert (uncurry (power1) (10.0,  3) == 1000.0)
  assert (uncurry (power1) (10.0,  4) == 10000.0)

  // Exercise 10

  def compose[A,B,C] (f: B => C, g: A => B) : A => C = (x:A) => f(g(x))
}
