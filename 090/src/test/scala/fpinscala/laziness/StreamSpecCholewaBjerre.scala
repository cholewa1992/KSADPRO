// Advanced Programming
// Andrzej Wasowski, IT University of Copenhagen

package fpinscala.laziness
import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

// If you comment out all the import lines below, then you test the Scala
// Standard Library implementation of Streams. Interestingly, the standard
// library streams are stricter than those from the book, so some laziness tests
// fail on them :)

import stream00._    // uncomment to test the book solution
// import stream01._ // uncomment to test the broken headOption implementation
// import stream02._ // uncomment to test another version that breaks headOption

class StreamSpecCholewaBjerre extends FlatSpec with Checkers {

  import Stream._

  behavior of "headOption"

  // a scenario test:

  it should "return None on an empty Stream (01)" in {
    assert(empty.headOption == None)
  }

  // An example generator of random finite non-empty streams
  def list2stream[A] (la: List[A]): Stream[A] = la.foldRight (empty[A]) (cons[A](_,_))

  def throwException[A]: A = throw new RuntimeException("was evaluated")
  def exceptionThrowingStream[A]: Stream[A] = throw new RuntimeException("was evaluated")

  // In ScalaTest we use the check method to switch to ScalaCheck's internal DSL
  def genNonEmptyStream[A] (implicit arbA: Arbitrary[A]): Gen[Stream[A]] =
    for { la <- arbitrary[List[A]] suchThat (_.nonEmpty)}
    yield list2stream (la)

  def infinitOf[A] (a: => A): Stream[A] = cons(a,infinitOf(a))
  def genInfinitStream[A] (implicit arbA: Arbitrary[A]): Gen[Stream[A]] = for(a <- arbitrary[A]) yield infinitOf(a)

  /* got this code at the exercise hour */
  def randomStream[A] (implicit arbA: Arbitrary[A]): Stream[A] =
    arbA.arbitrary.sample match {
      case Some(a) => cons(a,randomStream[A])
      case None => throw new RuntimeException("Not suppose to go here") 
    }

  def genInfinitRandomStream[A] (implicit arbA: Arbitrary[A]): Gen[Stream[A]] = Gen.const[Stream[A]] (randomStream)


  it should "return the head of the stream packaged in Some (02)" in check {
    // the implict makes the generator available in the context
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    ("singleton" |:
      Prop.forAll { (n :Int) => cons (n,empty).headOption == Some (n) } ) &&
    ("finite random stream" |:
      Prop.forAll { (s :Stream[Int]) => s.headOption != None } )
  }

  it should "should not force the tail of the stream (03)" in {
    val s = cons(1,exceptionThrowingStream[Int])
    assert(s.headOption == Some(1))
  }

  behavior of "take"
  it should "not force any heads nor any tails of the Stream it manipulates (04)" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (Gen.const[Stream[Int]] (cons(throwException[Int], exceptionThrowingStream[Int])))
    implicit def arbInt = Arbitrary[Int] (Gen.choose(1,Int.MaxValue))
    ("n = 0" |: Prop.forAll { (s: Stream[Int]) => s.take(0); true } )
    ("n > 0" |: Prop.forAll { (n: Int, s: Stream[Int]) => s.take(n); true } )
  }

  def streamOfNPlus[A] (n: Int) (a: => A) (implicit arbA: Arbitrary[A]): Stream[A] = arbA.arbitrary.sample match {
    case Some(a) => List.fill(n)(a).foldRight (cons(a,empty)) (cons[A](_,_))
    case None => Empty
  }

  it should "not force (n+1)st head ever (even if we force all elements of take(n)) (05)" in check {
    implicit def arbInt = Arbitrary[Int] (Gen.choose(1,10000))
    ("n = 0" |: Prop.forAll { (n: Int) => streamOfNPlus(0)(throwException[Int]).take(0).toList; true } )
    ("n > 0" |: Prop.forAll { (n: Int) => streamOfNPlus(n)(throwException[Int]).take(n).toList; true } )
  }

  it should "be idempotent (06)" in check {
    implicit def arbInt = Arbitrary[Int] (Gen.choose(1,10000))
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genInfinitRandomStream[Int])
    ("n = 0" |: Prop.forAll { (s: Stream[Int]) => s.take(0).take(0).toList == s.take(0).toList } )
    ("n > 0" |: Prop.forAll { (n: Int, s: Stream[Int]) => s.take(n).take(n).toList == s.take(n).toList } )
  }

  behavior of "drop"
  it should "be additive (07)" in check {
    implicit def arbInt = Arbitrary[Int] (Gen.choose(0,10000))
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    ("n = 0, m = 0" |: Prop.forAll { (s: Stream[Int]) => s.drop(0).drop(0).toList == s.drop(0).toList } )
    ("n = 0, m >= 0" |: Prop.forAll { (m:Int, s: Stream[Int]) => s.drop(0).drop(m).toList == s.drop(m).toList } )
    ("n >= 0, m = 0" |: Prop.forAll { (n: Int, s: Stream[Int]) => s.drop(n).drop(0).toList == s.drop(n).toList } )
    ("n >= 0, m >= 0" |: Prop.forAll { (n: Int, m:Int, s: Stream[Int]) => s.drop(n).drop(m).toList == s.drop(n+m).toList } )
  }

  it should "not force any of the dropped elements heads (08)" in check {
    implicit def arbInt = Arbitrary[Int] (Gen.choose(1,10000))
    implicit def arbIntStream = Arbitrary[Stream[Int]] (Gen.const(infinitOf(throwException[Int])))
    ("n = 0" |: Prop.forAll { (s: Stream[Int]) => s.drop(0); true } )
    ("n > 0" |: Prop.forAll { (n: Int, s: Stream[Int]) => s.drop(n); true } )
  }

  it should "not force any of the dropped elements heads, even if we evaluate tail (09)" in check {
    implicit def arbInt = Arbitrary[Int] (Gen.choose(1,10000))
    implicit def arbIntStream = Arbitrary[Stream[Int]] (Gen.const(infinitOf(throwException[Int])))
    ("n = 0" |: Prop.forAll { (s: Stream[Int]) => s.drop(0); true } )
    ("n > 0" |: Prop.forAll { (n: Int, s: Stream[Int]) => s.drop(n); true } )
  }

  behavior of "map"
  it should "not change when mapped with the identitiy function (10)" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    ("finite random stream" |: Prop.forAll { (s: Stream[Int]) => s.map(x => x).toList == s.toList } )
  }
  it should "terminate on infinite streams (11)" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genInfinitRandomStream[Int])
    ("infinit random stream" |: Prop.forAll { (s: Stream[Int]) => s.map(x => x); true } )
  }

  behavior of "append"
 
}
