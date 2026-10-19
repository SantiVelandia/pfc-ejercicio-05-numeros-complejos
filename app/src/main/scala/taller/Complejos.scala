package taller

/** Punto 4. Un número complejo r + i·i, con r la parte real e i la
  * imaginaria. Los objetos no cambian: cada operación devuelve uno nuevo.
  *
  * Tal como está compila y las pruebas quedan en rojo.
  */
class Complejos(val r: Double, val i: Double) {

  def +(otro: Complejos): Complejos = {new Complejos(r + otro.r, i + otro.i)}

  def -(otro: Complejos): Complejos = {new Complejos(r - otro.r, i - otro.i)}

  def *(otro: Complejos): Complejos = {new Complejos(r * otro.r - i * otro.i, r * otro.i + i * otro.r)}

  def /(otro: Complejos): Complejos ={
    val divisor = otro.r * otro.r + otro.i * otro.i
    require(divisor != 0, "no se puede dividir por cero")
    new Complejos((r * otro.r + i * otro.i) / divisor, (i * otro.r - r * otro.i) / divisor)
  }
  // "a + bi" con las dos partes redondeadas a tres decimales; si la parte
  // imaginaria es negativa, "a - bi".

  override def toString: String = {
    val real = math.round(r * 1000.0) / 1000.0
    val imaginaria = math.round(math.abs(i) * 1000.0) / 1000.0
    if (i < 0)
      real.toString + " - " + imaginaria.toString + "i"
    else
      real.toString + " + " + imaginaria.toString + "i"
  }
}

