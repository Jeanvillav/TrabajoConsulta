// Funcion de integracion con el metodo simpson 1/3
def integracion(f: Double => Double, a: Double, b: Double): Double = {
  val x = (a + b) / 2
  ((b - a) / 6) * (f(a) + 4 * f(x) + f(b))
}

// Aproximación
val inte1 = integracion(x => -x*x + 8*x - 12, 3, 5)
val inte2 = integracion(x => 3*x*x, 0, 2)
val inte3 = integracion(x => x + 2*x*x - x*x*x + 5*x*x*x*x, -1, 1)
val inte4 = integracion(x => (2*x + 1) / (x*x + x), 1, 2)
val inte5 = integracion(x => math.exp(x), 0, 1)
val inte6 = integracion(x => 1 / (x - 1), 2, 3)
val inte7 = integracion(x => 1 / (1 + x*x), 0, 1)

// Funcion para calcular el error para cada aproximación
def errAproximacion(valorEsp: Double, valorObt: Double): Double = {
  math.abs(valorEsp - valorObt)
}

// Calculo de errores
val err1 = errAproximacion(7.346666666666667, inte1)
val err2 = errAproximacion(8, inte2)
val err3 = errAproximacion(10.0/3.0, inte3)
val err4 = errAproximacion(math.log(3), inte4)
val err5 = errAproximacion(math.exp(1) - 1, inte5)
val err6 = errAproximacion(math.log(2), inte6)
val err7 = errAproximacion(math.Pi/4, inte7)
