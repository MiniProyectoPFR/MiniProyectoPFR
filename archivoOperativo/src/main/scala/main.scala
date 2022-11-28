package scala.math
val f1 = (n: Double) => -pow(n, 2) + (8 * n) - 12
val f2 = (n: Double) => 3 * (pow(n, 2))
val f3 = (n: Double) => n + (2 * (pow(n, 2))) - pow(n, 3) + (5 * pow(n, 4))
val f4 = (n: Double) => (2 * n + 1) / (pow(n, 2) + n)
val f5 = (n: Double) => pow(E,n)
val f6 = (n: Double) => 1 / (sqrt(n - 1))
val f7 = (n: Double) => 1 / (1 + pow(n, 2))

def simpUnTercio = (a: Int, b: Int, f: (Double) => Double) =>
  (b - a) * ((f(a) + (4 * f((a + b) / 2)) + f(b)) / 6)
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
@main def Hello(): Unit =
  println("-SIMPSON 1/3")
  println(simpUnTercio(3, 5, f1))
  println(simpUnTercio(0, 2, f2))
  println(simpUnTercio(-1, 1, f3))
  println(simpUnTercio(1, 2, f4))
  println(simpUnTercio(0, 1, f5))
  println(simpUnTercio(2, 3, f6))
  println(simpUnTercio(0, 1, f7))

  println("-SIMPSON 1/3 COMPUESTA")
  println("Ingrese el Valor de n: ")
  val n = scala.io.StdIn.readInt()
  println(simpUnTercioComp(3, 5, f1, n))
  println(simpUnTercioComp(0, 2, f2, n))
  println(simpUnTercioComp(-1, 1, f3, n))
  println(simpUnTercioComp(1, 2, f4, n))
  println(simpUnTercioComp(0, 1, f5, n))
  println(simpUnTercioComp(2, 3, f6, n))
  println(simpUnTercioComp(0, 1, f7, n))

  println("-SIMPSON 1/3 EXTENDIDA")
  println(simpUnTercioExt(3, 5, f1))
  println(simpUnTercioExt(0, 2, f2))
  println(simpUnTercioExt(-1, 1, f3))
  println(simpUnTercioExt(1, 2, f4))
  println(simpUnTercioExt(0, 1, f5))
  println(simpUnTercioExt(2, 3, f6))
  println(simpUnTercioExt(0, 1, f7))