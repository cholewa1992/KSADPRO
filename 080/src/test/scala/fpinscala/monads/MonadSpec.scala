// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen

// Example solutions for Monad exercises, using scalacheck
// Scalacheck's user guide:
// https://github.com/rickynils/scalacheck/wiki/User-Guide

package fpinscala.monads
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary
import scala.language.higherKinds
import Monad._


object  MonadSpec extends Properties("Monad[F[_]] laws..") {

  // Note: The law is fine, but remember that scalacheck has presently a very
  // weak function generator (only generates constant functions)
  def associative[A,F[_]] (m: Monad[F]) (implicit a: Arbitrary[F[A]]): Prop =
    forAll { (x: F[A], f: A => F[A], g: A => F[A]) =>
      m.flatMap[A,A] (m.flatMap[A,A] (x) (f)) (g) ==
      m.flatMap (x) (a => m.flatMap (f(a)) (g))
    }

  def identity[A, F[_]] (m: Monad[F]) (implicit arbFA: Arbitrary[F[A]],
    arbA: Arbitrary[A]): Prop =
      forAll { (x: F[A], f: A => F[A]) =>
      m.flatMap[A,A] (x) (m.unit[A] (_)) == x } :| "right unit" &&
    forAll { (y :A, f: A => F[A]) =>
      m.flatMap[A,A] (m.unit[A](y)) (f) == f(y) } :| "left unit"

  def monad[A,F[_]] (m :Monad[F]) (implicit arbFA: Arbitrary[F[A]],
    arbA: Arbitrary[A]) :Prop =
    associative[A,F] (m) && identity[A,F] (m)

  // uncomment when you have optionMonad
  property ("of optionMonad") = monad[Int,Option] (optionMonad)

  // Exercise 17
  property ("of streamMonad") = monad[Int,Stream] (streamMonad) && monad[String,Stream] (streamMonad)
  property ("of listMonad") = monad[Int,List] (listMonad) && monad[String,List] (listMonad)

}

object MonadKleisliSpec extends Properties("Monad[F[_]] kleisli laws..") {

  // Exercise 19
  def identity[A,B,F[_]] (m: Monad[F]) (implicit arbA: Arbitrary[A],
    arbFB: Arbitrary[F[B]]): Prop =
      forAll { (x: A, f: A => F[B]) => m.compose(m.unit(_:A),f)(x) == f(x) } :| "right unit" &&
      forAll { (x: A, f: A => F[B]) => m.compose(f,m.unit(_:B))(x) == f(x) } :| "left unit"

  def associative[A,B,C,D,F[_]] (m: Monad[F]) (implicit arbA: Arbitrary[A], 
    arbFB: Arbitrary[F[B]], arbFC: Arbitrary[F[C]], arbFD: Arbitrary[F[D]]): Prop = {
      forAll { (x: A, f: A => F[B], g: B => F[C], h: C => F[D]) =>
        m.compose(m.compose(f,g),h)(x) == m.compose(f,m.compose(g,h))(x) 
      }
    }

  def monad[A,B,C,D,F[_]] (m: Monad[F]) (implicit arbA: Arbitrary[A], 
    arbFB: Arbitrary[F[B]], arbFC: Arbitrary[F[C]], arbFD: Arbitrary[F[D]]) :Prop =
      identity[A,B,F] (m) && associative[A,B,C,D,F] (m)

  property ("of streamMonad with Int") = monad[Int,Int,Int,Int,Stream] (streamMonad) 
  property ("of streamMonad with String") = monad[String,String,String,String,Stream] (streamMonad)
  property (" of listMonad with Int") = monad[Int,Int,Int,Int,List] (listMonad) 
  property (" of listMonad with String") = monad[String,String,String,String,List] (listMonad)
  property ("of streamMonad with String to Int") = monad[String,Int,String,Int,Stream] (streamMonad) 
  property ("of listMonad with String to Int") = monad[String,Int,String,Int,List] (listMonad) 
  property ("of streamMonad with Int to Double") = monad[Int,Double,Int,Double,Stream] (streamMonad) 
  property ("of listMonad with Int to Double") = monad[Int,Double,Int,Double,List] (listMonad) 
  property ("of streamMonad with Double to Boolean") = monad[Double,Boolean,Double,Boolean,Stream] (streamMonad) 
  property ("of listMonad with Double to Boolean") = monad[Double,Boolean,Double,Boolean,List] (listMonad) 

  val m = streamMonad

def flatMapCheck[A,B,F[_]] (m: Monad[F]) (implicit arbFA: Arbitrary[F[A]], arbFB: Arbitrary[F[B]]): Prop = 
      forAll { (fa: F[A], f: A => F[B]) => m.flatMap (fa) (f) == m.flatMap_compose (fa) (f) }

  property ("flatMap_compose equals flatmap with listMonad and int to string") = flatMapCheck[Int,String,List] (listMonad)
  property ("flatMap_compose equals flatmap with streamMonad and int to string") = flatMapCheck[Int,String,Stream] (streamMonad)
}
