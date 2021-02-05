object Newton {

  def raiz_cuadrada(n:Int):Double={
    def f (x:Double): Double =Math.pow(x,2)-n

    def dx(x:Double): Double = 2 * x

    def s(x: Double): Boolean=Math.abs(x*x-n) / n < 0.0001

    def iterar(x:Double ):Double= x-f(x) / dx(x)

    def fin(x:Double):Double = if(s(x) )x else fin(iterar(x))

    fin(1.0)
  }

  def main(args: Array[String]): Unit = {
      print(raiz_cuadrada(25))
  }

}
