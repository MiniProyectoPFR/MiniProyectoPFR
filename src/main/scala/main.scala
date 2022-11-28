package scala.math

val f1 = (n: Double) => -pow(n, 2) + (8 * n) - 12
val f2 = (n: Double) => 3 * (pow(n, 2))
val f3 = (n: Double) => n + (2 * (pow(n, 2))) - pow(n, 3) + (5 * pow(n, 4))
val f4 = (n: Double) => (2 * n + 1) / (pow(n, 2) + n)
val f5 = (n: Double) => pow(E, 2)
val f6 = (n: Double) => 1 / (sqrt(n - 1))
val f7 = (n: Double) => 1 / (1 + pow(n, 2))

def simpUnTercio = (a: Int, b: Int, f: (Double) => Double) => (b - a) * ((f(a) + (4 * f((a + b) / 2)) + f(b)) / 6)
def simpUnTercioComp(a: Int, b: Int, f: (Double) => Double, n: Int): Double = {
  val h = (b - a) / n.toDouble
  val Xj = (j: Int) => a + j * h
  val fun = (j: Int) => f(Xj(2 * j - 2)) + (4 * f(Xj(2 * j - 1))) + f(Xj(2 * j))
  (h / 3) * ((1 to n / 2).map(fun(_)).sum)
}
def simpUnTercioExt(a: Int, b: Int, f: (Double) => Double): Double = {
  val n = 2 * (b - a)
  val h = (b - a) / n.toDouble
  val x = (n: Int) => f(a + n * h)
  (h / 3) * (f(a) + (4 * (1 to n - 1 by 2).map(x(_)).sum) + (2 * (2 to n - 2 by 2).map(x(_)).sum) + f(b))
}
@main def Hello: Unit =
  println(simpUnTercio(3, 5, f1))
  println(simpUnTercioExt(3, 5, f1))
  println(simpUnTercioComp(3, 5, f1, 100))
