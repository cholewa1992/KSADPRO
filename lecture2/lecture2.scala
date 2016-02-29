object Mymodule{

  def f2(n:Int):Int = {
    def frec (n:Int) (i:Int):Int = {
      println (i*2)
      if(i > 0) frec (n) (i-1)
      else n
    }
    frec (n) (n)
  }

  def f (n:Int):Int = {
    println(n*2)
    if(n > 0){
      f(n-1)
      n
    } else n
  }

  def main(args: Array[String]): Unit = {
    println (sum (List(1,2,3,4))) 
  }


  def sum (l:List[Int]):Int = {
    l match{
      case Nil => 0
      case x::xs => x + sum(xs)
    }
  }

  def foldRight[A,B] (f: (A,B) => B) (z:B) (l: List[A]) : B =
    l match {
      case x::xs => f(x,foldRight (f) (z) (xs))
      case Nil => z
    }

  def foldLeft[A,B] (f: (A,B) => B) (z:B) (l: List[A]) : B =
    l match {
      case x::xs => f(x,foldLeft (f) (z) (xs))
      case Nil =>
    }

  def sum2 (l:List[Int]):Int = foldRight[Int,Int] (_+_) (0) (List(1,2,3,4))
   
}
