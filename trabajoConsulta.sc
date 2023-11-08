def integracion(f: Double => Double, a: Double, b: Double, n: Int): Double = {
  val h = (b - a) / n
  val x0 = a
  val xn = b

  val sumatoria = (1 to n - 1).map(i => if (i % 2 == 0) 2 * f(x0 + i * h) else 4 * f(x0 + i * h)).sum

  (h / 3) * (f(x0) + sumatoria + f(xn))
}
val integral1 = integracion(x => x*x + 2*x - 1, 1, 3, 1000)
val integral2 = integracion(x => -x*x + 8*x - 12, 3, 5, 1000)

