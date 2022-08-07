package object planificacion {

  import datos._
  import scala.collection._
//----------------------------------------------- definicion tipos datos-----------------------------------------------
  type ListaVuelos= List[Vuelo]
  type Itinerario= ListaVuelos
//---------------------------------------------------------------------------------------------------------------------
  /**
   * Devuelve la lista de itinerarios posibles para viajar entre dos aeropuertos.
   * Por defecto, un itinerario no visita dos veces el mismo aeropuerto
   * @param a1 String, aeropuerto de salida
   * @param a2 String, aeropuerto de llegada
   * @return List, itinerarios posibles
   */

  def itinerarios(a1: String, a2: String): List[ListaVuelos] = {
    def definirVuelo(aeropuertoSalida: String, vuelos: List[Vuelo]): ListaVuelos = vuelos.filter((p: Vuelo) => p.Org.equals(aeropuertoSalida))
    def itinerarioDeVuelo(aeropuerto: String, vuelos: List[Vuelo]): List[ListaVuelos] = {
      if (vuelos.isEmpty) List(List())
      else {
        val vueloDirecto = definirVuelo(aeropuerto, vuelos).map(f => List(f))
        val itinerariosConEscala = for {
          aeropuertoInicio <- vueloDirecto.map(f => f.head)
          aeropuertoDestino <- itinerarioDeVuelo(aeropuertoInicio.Dst, vuelos.filterNot((p: Vuelo) => p.Org.equals(aeropuerto) || p.Dst.equals(aeropuerto)))
        } yield aeropuertoInicio::aeropuertoDestino
        vueloDirecto ++ itinerariosConEscala
      }
    }
    itinerarioDeVuelo(a1, vuelos).filter((p: List[Vuelo]) => p.last.Dst.equals(a2))
  }

  //-----------------------------funciones auxiliares para la función itinerariosTiempo--------------------------------

  /**
   * Halla la duración del vuelo (HH,MM)
   * @param HS hora salida
   * @param MS minutos salida
   * @param HL hora llegada
   * @param ML minutos llegada
   * @return duración (horas,minutos)
   */
  def tiempoNormal(HS: Int, MS: Int, HL: Int, ML: Int): (Int, Int)= {
    val duracion= {
      val min1= 60-(ML-MS)
      val min2= 60-(MS-ML)
      if(HS<HL){
        if(MS<ML)((HL-HS)-1, min1)
        else ((HL-HS)-1, min2)
      }else if (HS>HL){
        if(MS<ML)(23-HS+HL, min1)
        else (23-HS+HL, min2)
      }else{
        if(MS<ML)(24, min1)
        else (24, min2)
      }
    }
    duracion
  }

  /**
   * Halla la duración del vuelo (HH,MM)
   * @param HS hora salida
   * @param MS minutos salida
   * @param HL hora llegada
   * @param ML minutos llegada
   * @return duración (horas,minutos)
   */
  def diferenciaTiempo(h1: Int, m1: Int, h2: Int, m2: Int): (Int, Int) =
  {
    var diferencia: (Int, Int) = (0, 0)

    if (h1 == h2 && m1 == m2) {
      diferencia = (0, 0)
    } else if (h1 < h2) {
      diferencia = tiempoNormal(h1: Int, m1: Int, h2: Int, m2: Int)
    } else if (h1 > h2) {
      diferencia = calculadoraTiempo1((24, 0), tiempoNormal(h1, m1, h2, m2))
    } else if (h1 == h2 && m1 > m2) {
      diferencia = calculadoraTiempo1((24, 0), tiempoNormal(h2, h1, m2, m1))
    } else if (h1 == h2 && m1 <= m2) {
      diferencia = tiempoNormal(h1: Int, m1: Int, h2: Int, m2: Int)
    }
    diferencia
  }


  /**
   * Halla la duracion del avion en tierra
   * @param vuelos
   * @return
   */
  def diferenciaTiempoTierra(vuelos: List[Vuelo]): (Int, Int) = {
    if (vuelos.isEmpty || vuelos.length == 1) {
      (0, 0)
    } else {
      val hora1 = vuelos.head.HL
      val min1 = vuelos.head.ML
      val hora2 = vuelos(1).HS
      val min2 = vuelos(1).MS
      diferenciaTiempo(hora1, min1, hora2, min2)
    }
  }

  /**
   *
   * @param vuelos
   * @return
   */
  def tiempoTotalViaje(vuelos: List[Vuelo]): (Int, Int) = {

    var tiempo = (0, 0)

    for (i <- vuelos.indices) {

      val hora1 = vuelos(i).HS
      val min1 = vuelos(i).MS
      val hora2 = vuelos(i).HL
      val min2 = vuelos(i).ML

      if (i == vuelos.length - 1) {
        tiempo = calculadoraTiempo2(tiempo, diferenciaTiempo(hora1, min1, hora2, min2))
      } else {
        tiempo = calculadoraTiempo2(tiempo, calculadoraTiempo2(diferenciaTiempo(hora1, min1, hora2, min2),
          diferenciaTiempoTierra(List(vuelos(i), vuelos(i + 1)))))
      }

    }

    tiempo

  }

  /**
   *
   * @param uno
   * @param dos
   * @return
   */
  def calculadoraTiempo1(uno: (Int, Int), dos: (Int, Int)): (Int, Int) = {

    var hora = 0
    var minutos = 0
    var resultado = (0, 0)

    if (uno._2 - dos._2 < 0) {
      hora = (uno._1 - dos._1) - 1
      minutos = 60 - (dos._2 - uno._2)
      resultado = (hora, minutos)
    } else {
      hora = uno._1 - dos._1
      minutos = uno._2 - dos._2
      resultado = (hora, minutos)
    }

    resultado
  }

  /**
   *
   * @param uno
   * @param dos
   * @return
   */
  def calculadoraTiempo2(uno: (Int, Int), dos: (Int, Int)): (Int, Int) = {

    var hora = 0
    var minutos = 0
    var resultado = (0, 0)

    if (uno._2 + dos._2 >= 60) {
      hora = (uno._1 + dos._1) + 1
      minutos = (dos._2 + uno._2) - 60
      resultado = (hora, minutos)
    } else {
      hora = uno._1 + dos._1
      minutos = uno._2 + dos._2
      resultado = (hora, minutos)
    }

    resultado
  }

  //-------------------------------------------------------------------------------------------------------------------

  /**
   * Encuentra al menos tres itinerarios (si los hay) que corresponden a los menores
   * tiempos de viaje (contando tiempo de vuelo y tiempo de espera en tierra)
   * @param a1 String, aeropuerto salida
   * @param a2 String, aeropuerto llegada
   */
  def itinerariosTiempo(a1:String, a2:String): List[List[Vuelo]] =
  {
    val posiblesVuelos = itinerarios(a1,a2)
    var resultado: List[List[Vuelo]] = List()
    var tiempos = List[(List[Vuelo], (Int,Int))]()
    for (i <- posiblesVuelos.indices)
    {
      if(posiblesVuelos(i).length==1){
        val hora1 = posiblesVuelos(i).head.HS
        val min1 = posiblesVuelos(i).head.MS
        val hora2 = posiblesVuelos(i).head.HL
        val min2 = posiblesVuelos(i).head.ML
        tiempos = tiempos :+ (posiblesVuelos(i), diferenciaTiempo(hora1, min1, hora2, min2))
      }else{
        tiempos = tiempos :+ (posiblesVuelos(i), tiempoTotalViaje(posiblesVuelos(i)))
      }
    }
    tiempos = tiempos.sortBy(_._2)
    if(tiempos.length<3)
    {
      for(i <- tiempos.indices){
        resultado = resultado:+ tiempos(i)._1
      }
    }else{
      for(i <- 1 to 3){
        resultado = resultado:+ tiempos(i)._1
      }
    }
    resultado
  }


}
