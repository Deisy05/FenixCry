package object planificacion {

  import datos._
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
   * Halla la diferencia entre la hora de partida y la de llegada  (HH,MM)
   * @param HS hora salida
   * @param MS minutos salida
   * @param HL hora llegada
   * @param ML minutos llegada
   * @return duración (horas,minutos)
   */
  def lapsoTiempo(HS: Int, MS: Int, HL: Int, ML: Int): (Int, Int)= {
    val duracion= {

      if(HS<=HL){
        if(MS<=ML)((HL-HS), ML-MS)
        else ((HL-HS)-1, 60-(MS-ML))
      }else {
        if(MS<=ML)(24-(HS-HL),ML-MS)
        else (23-(HS-HL),60-(MS-ML) )
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
  def diferenciaTiempo(HS: Int, MS: Int, HL: Int, ML: Int): (Int, Int) =
  {
    var diferencia: (Int, Int) = (0, 0)

    if (HS == HL && MS == ML) {
      diferencia = (0, 0)
    } else if (HS < HL) {
      diferencia = lapsoTiempo(HS: Int, MS: Int, HL: Int, ML: Int)
    } else if (HS > HL) {
      diferencia = tiempo1((24, 0), lapsoTiempo(HS, MS, HL, ML))
    } else if (HS == HL && MS > ML) {
      diferencia = tiempo1((24, 0), lapsoTiempo(HL, HS, ML, MS))
    } else if (HS == HL && MS <= ML) {
      diferencia = lapsoTiempo(HS: Int, MS: Int, HL: Int, ML: Int)
    }
    diferencia
  }


  /**
   * Halla la duracion del avion en tierra
   * @param vuelos
   * @return
   */
  def tiempoTierra(vuelos: List[Vuelo]): (Int, Int) = {
    if (vuelos.isEmpty || vuelos.length == 1) {
      (0, 0)
    } else {
      val horaLlegada = vuelos.head.HL
      val minutosLlegada = vuelos.head.ML
      val horaSalida = vuelos(1).HS
      val minutosSalida = vuelos(1).MS
      diferenciaTiempo(horaLlegada, minutosLlegada, horaSalida, minutosSalida)
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

      val horaSalida = vuelos(i).HS
      val minutosSalida = vuelos(i).MS
      val horaLlegada = vuelos(i).HL
      val minutosLegada = vuelos(i).ML

      if (i == vuelos.length - 1) {
        tiempo = tiempo2(tiempo, diferenciaTiempo(horaSalida, minutosSalida, horaLlegada, minutosLegada))
      } else {
        tiempo = tiempo2(tiempo, tiempo2(diferenciaTiempo(horaSalida, minutosSalida, horaLlegada, minutosLegada),
          tiempoTierra(List(vuelos(i), vuelos(i + 1)))))
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
  def tiempo1(uno: (Int, Int), dos: (Int, Int)): (Int, Int) = {

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
   * Calcular la duración de varios vuelos
   * @param uno
   * @param dos
   * @return
   */
  def tiempo2(uno: (Int, Int), dos: (Int, Int)): (Int, Int) = {

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
