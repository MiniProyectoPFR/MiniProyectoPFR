package scala.math
def f1 = (n: Int) => -pow(n,2)+(8*n) -12
val f2 = (n: Int) => 3 * (pow(n,2))
val f3 = (n: Int) => n + (2*(pow(n,2))) - pow(n,3) + (5*pow(n,4))
val f4 = (n: Int) => (2*n +1)/(pow(n,2) + n)
val f5 = (n: Int) => pow(E,2)
val f6 = (n: Int) => 1/(sqrt(n - 1))
val f7 = (n: Int) => 1/(1+pow(n,2))
def simpUnTercio = (a: Int, b: Int ) => (b - a) * ((f1(a) + (4*f1((a+b)/2)) + f1(b))/6)
@main def Hello: Unit =
  print(simpUnTercio(3,5))
