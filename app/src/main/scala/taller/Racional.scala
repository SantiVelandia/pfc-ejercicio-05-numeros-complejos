package taller

/** Punto 3. Un número racional x/y guardado en su forma normalizada:
  * numerador y denominador sin factores comunes, el signo en el numerador y
  * el denominador siempre positivo. El cero se guarda como 0/1.
  *
  * Tal como está compila y las pruebas quedan en rojo.
  */
class Racional(x: Int, y: Int) {
  require(y!=0, "el denominador no puede ser cero")
  // El máximo común divisor de dos enteros no negativos.
  private def mcd(a: Int, b: Int): Int = {
    if (b == 0)
      a
    else
      mcd(b, a % b)
  } // Completar

  // Numerador y denominador ya normalizados.
  def numer: Int ={
    val nuevaX = math.abs(x)
    val nuevaY = math.abs(y)
    val divisor = mcd(nuevaX,nuevaY)
    if (nuevaX==0)
      0
    else if (x<0 ^ y<0)
      (nuevaX/divisor)*(-1)
    else (nuevaX/divisor)
  }
  def denom: Int = {
    val nuevaX = math.abs(x)
    val nuevaY = math.abs(y)
    val divisor = mcd(nuevaX,nuevaY)
    if (numer==0)
      1
    else (nuevaY/divisor)
  }

  def +(r: Racional): Racional = {
    new Racional((r.numer*denom)+(r.denom*numer),r.denom*denom)
  }

  def -(r: Racional): Racional = new Racional(numer*r.denom-r.numer*denom,r.denom*denom)

  def *(r: Racional): Racional = new Racional(r.numer*numer,r.denom*denom)

  def /(r: Racional): Racional = {
    require(r.numer != 0, "no se puede dividir por cero")
    new Racional(numer * r.denom, denom * r.numer)
  }

  // Si los dos racionales representan el mismo número.
  def ==(r: Racional): Boolean = {
    if (r.numer==numer && r.denom==denom)
      true
    else false
  }

  def <(r: Racional): Boolean = {
    numer*r.denom<r.numer*denom
  }

  // El mayor de los dos.
  def max(r: Racional): Racional = {
    if (this < r ) r
    else this
  }

  // "n/d", o solo "n" cuando el denominador es 1.
  override def toString: String = {
    if (denom == 1) numer.toString
    else ""+numer+"/"+denom+""
  }
}
