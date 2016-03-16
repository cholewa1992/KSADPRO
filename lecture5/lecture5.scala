sealed trait SomeTrait[+A]
case class State[S,+A] (run: S => (A,S))
case class Gen[+A] (sample: State[RNG,A])
