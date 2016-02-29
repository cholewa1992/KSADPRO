object MyModule {

  def abs(n: Int): Int = if(n < 0) -n else n

  private def formatAbs(x: Int) =
    s"The absolute value of $x is ${abs(x)}"

  val magic :Int = 42
  var result :Option[Int] = None

  def main(args: Array[String]): Unit = {
    //assert (magic - 84 == magic.-(84))
    //println(formatAbs(magic-100))
    //println(fact(5))  
    val mixed = List(-1,2,-3,4,-5)
    println (map (mixed) (fact _ compose abs _))
    println (map (mixed) (x => x+1))
  }

  def fact (n :Int) :Int = {
    if(n <= 1) 1
    else n * fact(n-1)
  }

  def factA (n:Int):Int = {
    def f(n:Int, r:Int):Int =
      if(n <= 1) r
      else f(n-1, n*r)
      f(n,1)
  }

  def map[A,B] (a:List[A]) (f:A => B):List[B] = {
    a match{
      case Nil => Nil
      case h::tail => f(h)::map (tail) (f)
    }
  }

  def mapA[A,B] (a:List[A]) (f:A => B):List[B] = {
    def m (a:List[A]) (b:List[B]):List[B] =
      a match{
        case Nil => b
        case h::tail => m (tail) (b ::: List(f(h)))
      }
    m (a) (List())
  }

}
