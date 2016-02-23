// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
//
// meant to be compiled, for example: fsc Stream.scala

package fpinscala.laziness

import Stream._

sealed trait Stream[+A] {

  def headOption () :Option[A] =
    this match {
      case Empty => None
      case Cons(h,t) => Some(h())
    }

  def tail :Stream[A] = this match {
    case Empty => Empty
    case Cons(h,t) => t()
  }

  def foldRight[B] (z : =>B) (f :(A, =>B) => B) :B = this match {
    case Empty => z
    case Cons (h,t) => f (h(), t().foldRight (z) (f))
    // Note 1. f can return without forcing the tail
    // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
    // if f requires to go deeply into the stream. So folds sometimes may be
    // less useful than in the strict case
  }

  // Note 1. eager; cannot be used to work with infinite streams. So foldRight
  // is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z : =>B) (f :(A, =>B) =>B) :B = this match {
    case Empty => z
    case Cons (h,t) => t().foldLeft (f (h(),z)) (f)
    // Note 2. even if f does not force z, foldLeft will continue to recurse
  }

  def exists (p : A => Boolean) :Boolean = this match {
    case Empty => false
    case Cons (h,t) => p(h()) || t().exists (p)
    // Note 1. lazy; tail is never forced if satisfying element found this is
    // because || is non-strict
    // Note 2. this is also tail recursive (because of the special semantics
    // of ||)
  }

  //def find (p :A => Boolean) :Option[A] = this.filter (p).headOption

  def toList :List[A] = this match {
    case Empty => List()
    case Cons(h,t) => h() :: (t()).toList
  }

  def take(n :Int) :Stream[A] = this match {
    case Cons(h,t) if n > 0 => Cons(() => h(), () => t().take(n-1))
    case _ => Empty
  }

  def drop(n :Int) :Stream[A] = this match {
    case Cons(h,t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true 
    case Cons(h,t) => if(p(h())) t().forAll (p) else false
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if(p(h())) => this
    case _ => Empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] = 
    this.foldRight[Stream[A]] (Empty) ((a,b) => if(p(a)) Cons(() => a, () => b) else b) 

  def map[B](f: A => B) :Stream[B] = 
    this.foldRight[Stream[B]] (Empty) ((a,b) => Cons(() => f(a), () => b))

  def filter(p: A => Boolean): Stream[A] = 
    this.foldRight[Stream[A]] (Empty) ((a,b) => if(p(a)) Cons(() => a, () => b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] = 
    this.foldRight[Stream[B]] (s) ((a,b) => Cons(() => a, () => b))

  def flatMap[B](f: A => Stream[B]) :Stream[B] =
    this.foldRight[Stream[B]] (Empty) ((a,b) => f(a).append(b))        
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]


object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]) :Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A] (as: A*) :Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
    // Note 1: ":_*" tells Scala to treat a list as multiple params
  // Note 2: pattern matching with :: does not seem to work with Seq, so we
  //         use a generic function API of Seq

  def to(n :Int) :Stream[Int] = if (n >= 0) cons(n, to(n-1)) else empty 
  def from(n :Int) :Stream[Int] = if (n >= 0) cons(n,from(n+1)) else empty 
  def naturals :Stream[Int] = from(0)

}

