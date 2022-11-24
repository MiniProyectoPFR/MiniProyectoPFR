package scala.math

def f1 = (n: Double) => -pow(n, 2) + (8 * n) - 12
def f2 = (n: Double) => 3 * (pow(n, 2))
val f3 = (n: Int) => n + (2 * (pow(n, 2))) - pow(n, 3) + (5 * pow(n, 4))
val f4 = (n: Int) => (2 * n + 1) / (pow(n, 2) + n)
val f5 = (n: Int) => pow(E, 2)
val f6 = (n: Int) => 1 / (sqrt(n - 1))
val f7 = (n: Int) => 1 / (1 + pow(n, 2))
def simpUnTercio = (a: Int, b: Int, f: (Double) => Double) => (b - a) * ((f(a) + (4 * f((a + b) / 2)) + f(b)) / 6)
//def n = (a: Int,b:Int) => 2 * (b-a)
//def h(a: Int,b:Int):Double = (b - a)/n(a,b)
//def simpUnTercioExt = (a: Int, b: Int, f:(Double) => Double )=> (h(a,b)/3)*(f(a)+(4*((1 to n(a,b)-1 by 2).map(i => a + (i * h(a,b)))).sum))
def simpUnTercioExt(a: Int, b: Int, f: (Double) => Double): Double = {
  val n = 2 * (b - a)
  val h = (b - a) / n.toDouble
  val x = (n: Int) => f(a + n * h)
  (h / 3) * ( f(a) + (4 * (1 to n - 1 by 2).map(x(_)).sum) + (2 * (2 to n - 2 by 2).map(x(_)).sum) + f(b) )
}
@main def Hello: Unit =
  //print(simpUnTercio(3,5,f1))
  println(simpUnTercioExt(3,5,f1))
  //println(simpUnTercio(0, 2, f2))
