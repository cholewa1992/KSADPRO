// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
//
// A script meant to be loaded into REPL (scala -i Main.scala)

import fpinscala.laziness._
import fpinscala.laziness.Stream._

// this is how we do simple interactive testing

val l1 :Stream[Int] = Empty
val l2 :Stream[Int] = empty

val l3 :Stream[Int]= cons(1, cons(2, cons (3, empty)))

println (l1.headOption)
println (l2.headOption)
println (l3.headOption)

val l5 :Stream[Int] = to(5) 
l3.toList
l5.toList

//This is fast becuase is it lazy and we therefore in the end only take 10 elements. The 10^9 intergers are never computed.
naturals.take(1000000000).drop(41).take(10).toList

//Because only 50 eleemnts will be checked due to the take
naturals.takeWhile(_<1000000000).drop(100).take(50).toList

naturals.forAll (_ < 0) //This will not fail because it will terminal instantly as first element is 1
//naturals.forAll (_ >= 0) //This will fail due to stackoverflow becaues it will keep iterating over the infinit stream never finding any elements breaking the predicate.

naturals.takeWhile2(_<1000000000).drop(100).take(50).toList

naturals.map (_*2).drop (30).take (50).toList
naturals.drop(42).filter (_%2 ==0).take (30).toList
naturals.append(naturals).append(naturals)
naturals.take(10).append(naturals).take(20).toList
naturals.flatMap (to _).take (100).toList
naturals.flatMap (x =>from (x)).take (100).toList

