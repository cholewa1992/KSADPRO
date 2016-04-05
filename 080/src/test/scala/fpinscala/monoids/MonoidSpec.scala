// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen
// Example solution for scala exercises using scalacheck
// Scalacheck's user guide:
// https://github.com/rickynils/scalacheck/wiki/User-Guide

package fpinscala.monoids
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

object MonoidSpec extends Properties("Monoids..") {

  import Monoid._

  def associative[A :Arbitrary] (m: Monoid[A]) :Prop =
    forAll { (a1: A, a2: A, a3: A) =>
      m.op(m.op(a1,a2), a3) == m.op(a1,m.op(a2,a3)) } :| "associativity"

  def unit[A :Arbitrary] (m :Monoid[A]) =
    forAll { (a :A) => m.op(a, m.zero) == a } :| "right unit" &&
    forAll { (a :A) => m.op(m.zero, a) == a } :| "left unit"

  def monoid[A :Arbitrary] (m :Monoid[A]) :Prop = associative (m) && unit (m)

  property ("stringMonoid is a monoid") = monoid (stringMonoid)

  // Exercise 4: test listMonoid, intAddition, intMultiplication, booleanOr,
  // booleanAnd and optionMonoid.

  property ("listMonoid is a monoid") = monoid (listMonoid[Int])
  property ("intAddition is a monoid") = monoid (intAddition)
  property ("intMultiplication is a monoid") = monoid (intMultiplication)
  property ("booleanOr is a monoid") = monoid (booleanOr)
  property ("booleanAnd is a monoid") = monoid (booleanAnd)
  property ("optionMoniod is a monoid") = monoid (optionMonoid[Int])
  //property ("endoMoniod is a monoid") = monoid (endoMonoid[Boolean])

  property ("foldMapV equals foldMap") = forAll { (v: IndexedSeq[Int]) =>
  foldMap (v.toList,intAddition) (i => i) == foldMapV(v,intAddition) (i => i) }

  // Exercise 7

  def homomorphic[A: Arbitrary,B: Arbitrary] (ma: Monoid[A]) (f: A => B) (mb: Monoid[B]) =
    forAll { (a1: A, a2: A) => mb.op(f(a1),f(a2)) == f(ma.op(a1,a2)) } :| "homomorphism"
  def isomorphic[A: Arbitrary,B: Arbitrary] (ma: Monoid[A]) (f1: A => B) (mb: Monoid[B]) (f2: B => A): Prop =
    homomorphic (ma) (f1) (mb) &&
    homomorphic (mb) (f2) (ma)

  property ("stringMonoid and listMonoid[Char] are isomorphic") = isomorphic (stringMonoid) (_.toList) (listMonoid[Char]) (_.mkString)

  // Exercise 8

  property ("booleanOr and booleanAnd are isomorphic") = isomorphic (booleanOr) (!_) (booleanAnd) (!_)

  // Exercise 9 (the testing part)

  property ("productMonoid is a monoid") = monoid (productMonoid (optionMonoid[Int]) (listMonoid[String]))
  //property ("functionMonoid is a monoid") = monoid (functionMonoid (intAddition))
}
