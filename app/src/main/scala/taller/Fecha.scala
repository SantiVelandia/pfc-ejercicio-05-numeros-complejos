package taller

/** Punto 2. Una fecha del calendario gregoriano: día, mes y año. El
  * constructor rechaza las fechas que no existen, así que todo objeto Fecha
  * es una fecha real.
  *
  * Tal como está compila y las pruebas quedan en rojo.
  */
class Fecha(val dia: Int, val mes: Int, val anio: Int) {
  require(mes >= 1 && mes <= 12)
  require(dia>=1&&dia<=diasDelMes(mes))

  // Si el año es bisiesto en el calendario gregoriano.
  private def esBisiesto: Boolean = {
    if ((anio%4==0 && anio%100 !=0) || (anio%400==0)) true
    else false
  }

  private def sumarMeses(m: Int,acc: Int): Int={
    if(m==mes)
      acc
    else sumarMeses(m+1,acc+diasDelMes(m))
  }
  // Cuántos días tiene el mes m de este año.
  private def diasDelMes(m: Int): Int = {
    if (m==1||m==3||m==5||m==7||m==8||m==10||m==12)
      31
    else if (m==4||m==6||m==9||m==11)
      30
    else if (esBisiesto) 29 else 28
  }

  // Qué número de día es esta fecha dentro de su año: el 1 de enero es 1.
  def diaDelAnio: Int ={
    sumarMeses(1,0)+dia
  }
  // La fecha del día siguiente.
  def siguiente: Fecha = {
    if (dia<diasDelMes(mes)) new Fecha(dia+1,mes,anio)
    else if (mes<12) new Fecha(1,mes+1,anio)
    else new Fecha(1,1,anio+1)
  }

  // La fecha n días después de esta, con n mayor o igual que 0.
  def masDias(n: Int): Fecha ={
    if (n==0) new Fecha(dia,mes,anio)
    else siguiente.masDias(n-1)
  }

  // Si esta fecha es anterior a otra.
  def antesDe(otra: Fecha): Boolean = {
    if (anio != otra.anio)
      anio < otra.anio
    else if (mes != otra.mes)
      mes < otra.mes
    else
      dia < otra.dia
  }

  // La forma "29/2/2024": día, mes y año separados por barras.
  override def toString: String = dia+"/"+mes+"/"+anio
}
