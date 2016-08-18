package map

import scala.util.Random._

import Domain._
import Domain.position._
import JSONReader._
import BreadthFirst._
import JSONUtilities._
import time.TimeMessages._

/*
 * I percorsi possono essere generati per un pedone, per un'automobile, per un autobus o per un tram
 * I percorsi di pedoni e macchine devono essere sufficientemente casuali da evitare intasamenti al crescere del numero di partecipanti
 * I pedoni devono avere un percorso ciclico che include:
 * - un houseplace
 * - un workplace
 * - un funplace
 * La giornata del pedone è scandita in questo modo:
 * - 8 ore a casa
 * - 8 ore a lavoro
 * - 8 ore a svago
 * Il percorso non dichiara solamente le tappe da effettuare, ma anche gli orari di fine "periodo"
 * - Il primo orario determina quando si finisce di stare nell'houseplace, dunque il pedone inizierà
 * a raggiungere il workplace
 * - Il secondo orario determina quando si finisce di stare al workplace, dunque il pedone inizierà
 * a raggiungere il fun place
 * - Il terzo orario determina quando si finisce di stare al funplace, dunque il pedone inizierà a
 * raggiungere il houseplace
 * Qualora scattasse l'orario dell'attività successiva nel tragitto per raggiungere il posto dell'attività precedente,
 * il pedone raggiunge comunque il posto iniziale per poi ripartire immediatamente verso la nuova destinazione
 * Le automobili seguono lo stesso tipo di vita, svolgono le tre attività con lo stesso comportamento
 * Bus e tram hanno un percorso ciclico basato sulla loro tratta
 * La mappa è creata in modo tale che le fermate siano etichettate in base alla tratta e siano coerenti con un unico verso di percorrenza (orario od antiorario)
 * Bus e tram continuano la loro tratta ciclica in loop
 */

object Routes {
  
  val map = JSONReader.readAll("map.json")
  
  class BusRouteVertexNotFoundException extends Exception
  class TramRouteVertexNotFoundException extends Exception
  class NoSlicesAvailableException extends Exception
  
  case class direction(position : position, beginToEnd : Boolean)
  
  trait step
  case class road_step(road : road, direction : direction) extends step
  case class lane_step(lane : lane, direction : direction) extends step
  case class crossroad_step(crossroad : crossroad, direction : direction) extends step
  case class pedestrian_crossroad_step(pedestrian_crossroad : pedestrian_crossroad, direction : direction) extends step
  case class bus_stop_step(bus_stop : bus_stop, direction : direction, ignore : Boolean) extends step
  case class tram_stop_step(tram_stop : tram_stop, direction : direction, ignore : Boolean) extends step
  case class zone_step(zone : zone, direction : direction) extends step
  
  trait route
  case class pedestrian_route(houseEndTime : TimeValue, houseToWorkRoute : List[step], workEndTime : TimeValue, workToFunRoute : List[step], funEndTime : TimeValue, funToHomeRoute : List[step]) extends route
  case class car_route(houseEndTime : TimeValue, houseToWorkRoute : List[step], workEndTime : TimeValue, workToFunRoute : List[step], funEndTime : TimeValue, funToHomeRoute : List[step]) extends route
  case class bus_route(route : List[step]) extends route
  case class tram_route(route : List[step]) extends route
  
  /*
   * I PEZZI DI PERCORSO DI PEDONI E MACCHINE SEGUONO QUESTA LOGICA:
   * - INCLUDONO LA ZONA DI PARTENZA
   * - NON INCLUDONO LA ZONA DI DESTINAZIONE
   * LE TRATTE DI BUS E TRAM INIZIANO SEMPRE CON UNA DELLE STAZIONI
   */
  
  /*
   * Funzione per la creazione di un percorso per pedone
   * Sceglie le tre destinazioni (home, work e fun) casualmente e genera i percorsi per raggiungerli
   * Quando possibile, prende un mezzo pubblico (al massimo una volta per ogni pezzo, home->work, work->fun, fun->home)
   */
  def createPedestrianRoute() : (pedestrian_route, Boolean) = {
    // oltre al percorso, restituisce un flag
    // se il flag è a true, allora il percorso include l'utilizzo di un mezzo pubblico in uno dei tre pezzi
    // crea i tempi
    val times = createTimes()
    // crea le destinazioni
    val places = createPlaces()
    return createPedestrianRoute(places, times)
  }
  
  def createPedestrianRoute(places : (String, String, String), times : (TimeValue, TimeValue, TimeValue)) : (pedestrian_route, Boolean) = {
    // ottieni i tre pezzi di percorso
    val firstRoute = pedestrianBreadthFirstSearch(map, places._1, places._2)
    val secondRoute = pedestrianBreadthFirstSearch(map, places._2, places._3)
    val thirdRoute = pedestrianBreadthFirstSearch(map, places._3, places._1)
    // per ciascuno dei pezzi di percorso, trova la massima sottosequenza comune con una tratta del bus o del tram
    val busRoutesList = getAllBusRoutes()
    val tramRoutesList = getAllTramRoutes()
    val busRoutesSlices = busRoutesList.map { bus_route => getAllPossibleSlices(bus_route) }
    val tramRoutesSlices = tramRoutesList.map { tram_route => getAllPossibleSlices(tram_route) }
    val firstRouteReplaced = routeReplacement(firstRoute, busRoutesSlices, tramRoutesSlices)
    val secondRouteReplaced = routeReplacement(secondRoute, busRoutesSlices, tramRoutesSlices)
    val thirdRouteReplaced = routeReplacement(thirdRoute, busRoutesSlices, tramRoutesSlices)
    // flag per segnalare se il percorso include nel complesso l'utilizzo di un mezzo pubblico
    val flag = (firstRoute != firstRouteReplaced) || (secondRoute != secondRouteReplaced) || (thirdRoute != thirdRouteReplaced)
    return (pedestrian_route(times._1, firstRouteReplaced, times._2, secondRouteReplaced, times._3, thirdRouteReplaced), flag)
  }
  
  /*
   * Funzione per la creazione di un percorso per una macchina
   * Sceglie le tre destinazioni (home, work e fun) casualmente e genera i percorsi per raggiungerli
   */
  def createCarRoute() : car_route = {
    // crea i tempi
    val times = createTimes()
    // crea le destinazioni
    val places = createPlaces()
    return createCarRoute(places, times)
  }
  
  def createCarRoute(places : (String, String, String), times : (TimeValue, TimeValue, TimeValue)) : car_route = {
    // ottieni i tre pezzi di percorso
    val firstRoute = carBreadthFirstSearch(map, places._1, places._2)
    val secondRoute = carBreadthFirstSearch(map, places._2, places._3)
    val thirdRoute = carBreadthFirstSearch(map, places._3, places._1)
    return car_route(times._1, firstRoute, times._2, secondRoute, times._3, thirdRoute)
  }
  
  /*
   * Funzione per la creazione del percorso di un bus, sulla base del numero della tratta in input
   */
  def createBusRoute(route : Int) : bus_route = {
    var bus_route_steps = List[step]()
    // noto il numero della tratta, bisogna recuperare tutti gli step
    val firstStop = map.bus_stops.filter { bus_stop => bus_stop.route == route }.head
    var firstDirection = direction(position.up, true) // valori dummy
    firstStop.position match {
      case position.up =>
        firstDirection = direction(position.up, false)
      case position.down =>
        firstDirection = direction(position.down, true)
      case position.left =>
        firstDirection = direction(position.left, false)
      case position.right =>
        firstDirection = direction(position.right, true)
    }
    bus_route_steps = bus_route_steps :+ bus_stop_step(firstStop, firstDirection, false)
    var neighbor = "" // dummy
    if(firstDirection.beginToEnd == false) {
      neighbor = firstStop.coordinates.begin.id
    }
    else {
      neighbor = firstStop.coordinates.end.id
    }
    while(neighbor != firstStop.id) {
      val (entity, _, _, _) = splitId(neighbor)
      entity match {
        case "road" =>
          // bisogna trovare la lane (deve esistere) che ha come route quella in input
          val laneId = getRoad(map, neighbor).get.lanesIDs.filter { lane => getLane(map, lane).get.bus_routes.contains(route) }.head
          val lane = getLane(map, laneId).get
          // la direzione è la stessa dell'ingresso
          val direction = getStepDirection(bus_route_steps.last)
          bus_route_steps = bus_route_steps :+ lane_step(lane, direction)
          if(direction.beginToEnd == true) {
            neighbor = getRoad(map, lane.road).get.coordinates.end.id
          }
          else {
            neighbor = getRoad(map, lane.road).get.coordinates.begin.id
          }
        case "crossroad" =>
          // bisogna trovare il vicino che appartiene alla tratta
          // può essere una strada, ma potrebbe essere un incrocio
          // in tal caso bisogna selezionare il vicino che ha tra i suoi vicini una strada che appartiene alla tratta
          // poi bisogna decretare la direzione concordemente col vicino selezionato
          val crossroad = getCrossroad(map, neighbor).get
          val roadsVertexes = crossroad.vertexes.filter { vertex => getRoad(map, vertex.id) != None }
          var routeRoadsVertexes = roadsVertexes.filter { roadVertex => getRoad(map, roadVertex.id).get.lanesIDs.filter { laneId => getLane(map, laneId).get.bus_routes.contains(route) }.length > 0 }
          // bisogna rimuovere l'eventuale strada da cui siamo venuti
          routeRoadsVertexes = routeRoadsVertexes.filter { vertex => getRoad(map, vertex.id).get.lanesIDs.contains(getStepId(bus_route_steps.last)) == false }
          if(routeRoadsVertexes.isEmpty == true) {
            // bisogna cercare negli incroci confinanti
            val crossroadVertex = getBusRouteVertex(route, crossroad.vertexes)
            // bisogna stabilire la direzione, decretato il vertice di uscita
            val direction = getCrossroadExitingDirection(crossroad, getStepDirection(bus_route_steps.last), crossroadVertex)
            bus_route_steps = bus_route_steps :+ crossroad_step(crossroad, direction)
            neighbor = crossroadVertex.id
          }
          else {
            // deve esserci una sola strada con tali caratteristiche
            assert(routeRoadsVertexes.length == 1)
            // bisogna stabilire la direzione, decretato il vertice di uscita
            val direction = getCrossroadExitingDirection(crossroad, getStepDirection(bus_route_steps.last), routeRoadsVertexes(0))
            bus_route_steps = bus_route_steps :+ crossroad_step(crossroad, direction)
            neighbor = routeRoadsVertexes(0).id
          }
        case "pedestrian_crossroad" =>
          val pedestrian_crossroad = getPedestrianCrossroad(map, neighbor).get
          val direction = getStepDirection(bus_route_steps.last)
          bus_route_steps = bus_route_steps :+ pedestrian_crossroad_step(pedestrian_crossroad, direction)
          if(direction.beginToEnd == true) {
            neighbor = pedestrian_crossroad.coordinates.end.id
          }
          else {
            neighbor = pedestrian_crossroad.coordinates.begin.id
          }
        case "bus_stop" =>
          val bus_stop = getBusStop(map, neighbor).get
          val direction = getStepDirection(bus_route_steps.last)
          if(bus_stop.route == route) {
            bus_route_steps = bus_route_steps :+ bus_stop_step(bus_stop, direction, false)
          }
          else {
            bus_route_steps = bus_route_steps :+ bus_stop_step(bus_stop, direction, true)
          }
          if(direction.beginToEnd == true) {
            neighbor = bus_stop.coordinates.end.id
          }
          else {
            neighbor = bus_stop.coordinates.begin.id
          }
        case "tram_stop" =>
          val tram_stop = getTramStop(map, neighbor).get
          val direction = getStepDirection(bus_route_steps.last)
          bus_route_steps = bus_route_steps :+ tram_stop_step(tram_stop, direction, true)
          if(direction.beginToEnd == true) {
            neighbor = tram_stop.coordinates.end.id
          }
          else {
            neighbor = tram_stop.coordinates.begin.id
          }
      }
    }
    return bus_route(bus_route_steps)
  }
  
  /*
   * Funzione per la creazione del percorso di un tram, sulla base del numero della tratta in input
   */
  def createTramRoute(route : Int) : tram_route = {
    var tram_route_steps = List[step]()
    // noto il numero della tratta, bisogna recuperare tutti gli step
    val firstStop = map.tram_stops.filter { tram_stop => tram_stop.route == route }.head
    var firstDirection = direction(position.up, true) // valori dummy
    firstStop.position match {
      case position.up =>
        firstDirection = direction(position.up, false)
      case position.down =>
        firstDirection = direction(position.down, true)
      case position.left =>
        firstDirection = direction(position.left, false)
      case position.right =>
        firstDirection = direction(position.right, true)
    }
    tram_route_steps = tram_route_steps :+ tram_stop_step(firstStop, firstDirection, false)
    var neighbor = "" // dummy
    if(firstDirection.beginToEnd == false) {
      neighbor = firstStop.coordinates.begin.id
    }
    else {
      neighbor = firstStop.coordinates.end.id
    }
    while(neighbor != firstStop.id) {
      val (entity, _, _, _) = splitId(neighbor)
      entity match {
        case "road" =>
          // bisogna trovare la lane (deve esistere) che ha come tram = true
          val laneId = getRoad(map, neighbor).get.lanesIDs.filter { lane => getLane(map, lane).get.tram == true }.head
          val lane = getLane(map, laneId).get
          // la direzione è la stessa dell'ingresso
          val direction = getStepDirection(tram_route_steps.last)
          tram_route_steps = tram_route_steps :+ lane_step(lane, direction)
          if(direction.beginToEnd == true) {
            neighbor = getRoad(map, lane.road).get.coordinates.end.id
          }
          else {
            neighbor = getRoad(map, lane.road).get.coordinates.begin.id
          }
        case "crossroad" =>
          // bisogna trovare il vicino che appartiene alla tratta
          // può essere una strada, ma potrebbe essere un incrocio
          // in tal caso bisogna selezionare il vicino che ha tra i suoi vicini una strada che appartiene alla tratta
          // poi bisogna decretare la direzione concordemente col vicino selezionato
          val crossroad = getCrossroad(map, neighbor).get
          val roadsVertexes = crossroad.vertexes.filter { vertex => getRoad(map, vertex.id) != None }
          var routeRoadsVertexes = roadsVertexes.filter { roadVertex => getRoad(map, roadVertex.id).get.lanesIDs.filter { laneId => getLane(map, laneId).get.tram == true }.length > 0 }
          // bisogna rimuovere l'eventuale strada da cui siamo venuti
          routeRoadsVertexes = routeRoadsVertexes.filter { vertex => getRoad(map, vertex.id).get.lanesIDs.contains(getStepId(tram_route_steps.last)) == false }
          if(routeRoadsVertexes.isEmpty == true) {
            // bisogna cercare negli incroci confinanti
            val crossroadVertex = getTramRouteVertex(crossroad.vertexes)
            // bisogna stabilire la direzione, decretato il vertice di uscita
            val direction = getCrossroadExitingDirection(crossroad, getStepDirection(tram_route_steps.last), crossroadVertex)
            tram_route_steps = tram_route_steps :+ crossroad_step(crossroad, direction)
            neighbor = crossroadVertex.id
          }
          else {
            // deve esserci una sola strada con tali caratteristiche
            assert(routeRoadsVertexes.length == 1)
            // bisogna stabilire la direzione, decretato il vertice di uscita
            val direction = getCrossroadExitingDirection(crossroad, getStepDirection(tram_route_steps.last), routeRoadsVertexes(0))
            tram_route_steps = tram_route_steps :+ crossroad_step(crossroad, direction)
            neighbor = routeRoadsVertexes(0).id
          }
        case "pedestrian_crossroad" =>
          val pedestrian_crossroad = getPedestrianCrossroad(map, neighbor).get
          val direction = getStepDirection(tram_route_steps.last)
          tram_route_steps = tram_route_steps :+ pedestrian_crossroad_step(pedestrian_crossroad, direction)
          if(direction.beginToEnd == true) {
            neighbor = pedestrian_crossroad.coordinates.end.id
          }
          else {
            neighbor = pedestrian_crossroad.coordinates.begin.id
          }
        case "bus_stop" =>
          val bus_stop = getBusStop(map, neighbor).get
          val direction = getStepDirection(tram_route_steps.last)
          tram_route_steps = tram_route_steps :+ bus_stop_step(bus_stop, direction, true)
          if(direction.beginToEnd == true) {
            neighbor = bus_stop.coordinates.end.id
          }
          else {
            neighbor = bus_stop.coordinates.begin.id
          }
        case "tram_stop" =>
          val tram_stop = getTramStop(map, neighbor).get
          val direction = getStepDirection(tram_route_steps.last)
          if(tram_stop.route == route) {
            tram_route_steps = tram_route_steps :+ tram_stop_step(tram_stop, direction, false)
          }
          else {
            tram_route_steps = tram_route_steps :+ tram_stop_step(tram_stop, direction, true)
          }
          if(direction.beginToEnd == true) {
            neighbor = tram_stop.coordinates.end.id
          }
          else {
            neighbor = tram_stop.coordinates.begin.id
          }
      }
    }
    return tram_route(tram_route_steps)
  }
  
  
  
  /*
   * Genera tre zone casualmente, con i vincoli che:
   * 1) le zone siano una home, una work e una fun
   * 2) non vi siano due posti sulla stessa strada
   */
  def createPlaces() : (String,String,String) = {
    // scegli un houseplace a caso
    var houseplace = shuffle(getAllZones(map).filter { zone => zone.variety == variety.houseplace }).head
    // scegli un workplace a caso
    var workplace = shuffle(getAllZones(map).filter { zone => zone.variety == variety.workplace }).head
    // scegli un funplace a caso
    var funplace = shuffle(getAllZones(map).filter { zone => zone.variety == variety.funplace }).head
    // non accettare i punti se almeno due di essi risiedono sulla stessa strada
    var ok = false
    while(ok == false) {
      if((houseplace.road == workplace.road) || (workplace.road == funplace.road) || (houseplace.road == funplace.road)) {
        houseplace = shuffle(getAllZones(map).filter { zone => zone.variety == variety.houseplace }).head
        workplace = shuffle(getAllZones(map).filter { zone => zone.variety == variety.workplace }).head
        funplace = shuffle(getAllZones(map).filter { zone => zone.variety == variety.funplace }).head
      }
      else {
        ok = true
      }
    }
    return (houseplace.id, workplace.id, funplace.id)
  }
  
  /*
   * Ritorna l'id dell'entità attraversata allo step in input
   */
  def getStepId(step : step) : String = {
    step match {
      case road_step(road, _) => return road.id
      case lane_step(lane, _) => return lane.id
      case crossroad_step(crossroad, _) => return crossroad.id
      case pedestrian_crossroad_step(pedestrian_crossroad, _) => return pedestrian_crossroad.id
      case bus_stop_step(bus_stop, _, _) => return bus_stop.id
      case tram_stop_step(tram_stop, _, _) => return tram_stop.id
      case zone_step(zone, _) => return zone.id
    }
  }
  
  /*
   * Ritorna la direzione di uscita dello step in input
   * Ad esempio, se lo step in input considera una strada, la direzione dello step
   * è la direzione di uscita dalla strada in questione, cioè la direzione di ingresso
   * per la successiva entità
   */
  def getStepDirection(step : step) : direction = {
    step match {
      case road_step(_, direction) => return direction
      case lane_step(_, direction) => return direction
      case crossroad_step(_, direction) => return direction
      case pedestrian_crossroad_step(_, direction) => return direction
      case bus_stop_step(_, direction, _) => return direction
      case tram_stop_step(_, direction, _) => return direction
      case zone_step(_, direction) => return direction
    }
  }
  
  /*
   * Funzione di utilità
   * Permette di trovare il vertice corrispondente alla tratta del bus quando vi sono incroci adiacenti
   * A partire dalla tratta e dalla lista di vertici in input, scorre i neighbor dei vertici al fine di trovare
   * un incrocio: una volta trovato un incrocio, cerca tra i vertici dell'incrocio trovato una road le cui lanes
   * (una delle lanes) appartengano alla tratta in input 
   */
  def getBusRouteVertex(route : Int, vertexes : List[vertex]) : vertex = {
    // tutti i vertici dell'incrocio, escluso quello di arrivo, non corrispondono a strade le cui corsie (una) sono utilizzate dalla tratta
    // bisogna cercare tra i vertici che corrispondono ad incroci se una delle loro strade corrisponde a una della tratta
    // in tal caso, restituire il vertice corrispondente
    for(vertex <- vertexes if vertex.id != "nil") {
      val (entity, _, _, _) = splitId(vertex.id)
      entity match {
        case "crossroad" =>
          // unico caso di interesse
          val crossroad = getCrossroad(map, vertex.id).get
          val routeVertexes = crossroad.vertexes.filter { vert =>  
            // il vertice deve corrispondere ad una strada
            getRoad(map, vert.id) != None &&
            // tra le corsie della strada ve ne è una della tratta
            getRoad(map, vert.id).get.lanesIDs.filter { laneId => getLane(map, laneId).get.bus_routes.contains(route) }.length > 0}
          assert(routeVertexes.length == 1)
          return vertex
        case _ =>
      }
    }
    // non si dovrebbe arrivare qui
    throw new BusRouteVertexNotFoundException
  }
  
  /*
   * Funzione di utilità
   * Permette di trovare il vertice corrispondente alla tratta del tram quando vi sono incroci adiacenti
   * A partire dalla tratta e dalla lista di vertici in input, scorre i neighbor dei vertici al fine di trovare
   * un incrocio: una volta trovato un incrocio, cerca tra i vertici dell'incrocio trovato una road le cui lanes
   * (una delle lanes) appartengano alla tratta in input 
   */
  def getTramRouteVertex(vertexes : List[vertex]) : vertex = {
    // tutti i vertici dell'incrocio, escluso quello di arrivo, non corrispondono a strade le cui corsie (una) sono utilizzate dalla tratta
    // bisogna cercare tra i vertici che corrispondono ad incroci se una delle loro strade corrisponde a una della tratta
    // in tal caso, restituire il vertice corrispondente
    for(vertex <- vertexes) {
      val (entity, _, _, _) = splitId(vertex.id)
      entity match {
        case "crossroad" =>
          // unico caso di interesse
          val crossroad = getCrossroad(map, vertex.id).get
          val routeVertexes = crossroad.vertexes.filter { vert =>  
            // il vertice deve corrispondere ad una strada
            getRoad(map, vert.id) != None &&
            // tra le corsie della strada ve ne è una della tratta
            getRoad(map, vert.id).get.lanesIDs.filter { laneId => getLane(map, laneId).get.tram == true }.length > 0}
          assert(routeVertexes.length == 1)
          return vertex
        case _ =>
      }
    }
    // non si dovrebbe arrivare qui
    throw new TramRouteVertexNotFoundException
  }
  
  /*
   * Funzione di utilità per bus e tram
   * Dato l'incrocio, la direzione di arrivo all'incrocio e il vertice di uscita dall'incrocio,
   * determina la direzione di uscita dall'incrocio
   */
  def getCrossroadExitingDirection(crossroad : crossroad, arriving_direction : direction, exiting_vertex : vertex) : direction = {
		  // capisci il vertice di arrivo
		  // verso dx, dal basso => vertice più a sinistra
		  // verso sx, dall'alto => vertice più a destra
		  // verso l'alto, da destra => vertice più in basso
		  // verso il basso, da sinistra => vertice più in alto
		  var arriving_vertex = vertex(point(0, 0), "") // dummy
		  if(arriving_direction.beginToEnd == true) {
			  arriving_direction.position match {
				  case position.down =>
					  arriving_vertex = crossroad.vertexes.sortBy { vert => vert.point.x }.head
					case position.right =>
					  arriving_vertex = crossroad.vertexes.sortBy { vert => vert.point.y }.head
					case _ =>
					  throw new WrongDirectionException
					}
      }
			else {
			  arriving_direction.position match {
				  case position.up =>
					  arriving_vertex = crossroad.vertexes.sortBy { vert => vert.point.x }.reverse.head
				  case position.left =>
					  arriving_vertex = crossroad.vertexes.sortBy { vert => vert.point.y }.reverse.head
				  case _ =>
					  throw new WrongDirectionException
			  }
			}
		  // verso dx (beginToEnd), dal basso =>
		  //   se il vertice ha la stessa y, stessa direzione
		  //   se il vertice ha y più bassa, verso il basso a sinistra
		  //   se il vertice ha y più alta, verso l'alto a destra
		  // verso sx (!beginToEnd), dall'alto =>
		  //   se il vertice ha la stessa y, stessa direzione
		  //   se il vertice ha y più bassa, verso il basso a sinistra
		  //   se il vertice ha y più alta, verso l'alto a destra
		  // verso l'alto (beginToEnd), da destra =>
		  //   se il vertice ha la stessa x, stessa direzione
		  //   se il vertice ha x più bassa, verso sinistra in alto
		  //   se il vertice ha x più alta, verso destra in basso
		  // verso il basso (!beginToEnd), da sinistra =>
		  //   se il vertice ha la stessa x, stessa direzione
		  //   se il vertice ha x più bassa, verso sinistra in alto
		  //   se il vertice ha x più alta, verso destra in basso
		  arriving_direction.position match {
		  case position.down | position.up =>
		  if(exiting_vertex.point.y == arriving_vertex.point.y) {
			  return arriving_direction
		  }
		  else if(exiting_vertex.point.y < arriving_vertex.point.y) {
			  return direction(position.left, false)
		  }
		  else {
			  return direction(position.right, true)
		  }
		  case position.left | position.right =>
		  if(exiting_vertex.point.x == arriving_vertex.point.x) {
			  return arriving_direction
		  }
		  else if(exiting_vertex.point.x < arriving_vertex.point.x) {
			  return direction(position.up, false)
		  }
		  else {
			  return direction(position.down, true)
		  }
		}
  }
  
  /*
   * Data una tratta di bus o tram, ritorna tutt i possibili pezzi di tratta
   * Esempio: data la tratta 1 - 2 - 3 - 4, dove i numeri sono fermate,
   * la funzione restituisce
   * 1 - 2
   * 1 - 3
   * 1 - 4
   * 2 - 3
   * 2 - 4
   * 2 - 1
   * ...
   */
  def getAllPossibleSlices(route : route) : List[List[step]] = {
    route match {
      case bus_route(sequence) =>
        // trova tutte le fermate del bus (non ignorate)
        var indexes = List[Int]()
        for(step <- sequence) {
          step match {
            case bus_stop_step(_, _, ignore) =>
              if(ignore == false) {
                indexes = indexes :+ sequence.indexOf(step)
              }
            case _ =>
          }
        }
        var slices = List[List[step]]()
        for(index <- indexes) {
          for(index_2 <- indexes if index_2 != index) {
            if(index_2 > index) {
              slices = slices :+ sequence.slice(index, index_2+1)
            }
            else {
              val firstPiece = sequence.slice(index, sequence.length)
              val secondPiece = sequence.slice(0, index_2+1)
              slices = slices :+ (firstPiece ++ secondPiece)
            }
          }
        }
        return slices
      case tram_route(sequence) =>
        // trova tutte le fermate del tram (non ignorate)
        var indexes = List[Int]()
        for(step <- sequence) {
          step match {
            case tram_stop_step(_, _, ignore) =>
              if(ignore == false) {
                indexes = indexes :+ sequence.indexOf(step)
              }
            case _ =>
          }
        }
        var slices = List[List[step]]()
        for(index <- indexes) {
          for(index_2 <- indexes if index_2 != index) {
            if(index_2 > index) {
              slices = slices :+ sequence.slice(index, index_2+1)
            }
            else {
              val firstPiece = sequence.slice(index, sequence.length)
              val secondPiece = sequence.slice(0, index_2+1)
              slices = slices :+ (firstPiece ++ secondPiece)
            }
          }
        }
        return slices
      case _ =>
        throw new NoSlicesAvailableException
    }
  }
  
  /*
   * Ritorna tutte le tratte (percorsi) del bus della mappa
   */
  def getAllBusRoutes() : List[bus_route] = {
    var maxRoute = -1
    for(bus_stop <- map.bus_stops) {
      if(bus_stop.route > maxRoute) {
        maxRoute = bus_stop.route
      }
    }
    var busRoutesList = List[bus_route]()
    for(route <- 1 to maxRoute) {
      busRoutesList = busRoutesList :+ createBusRoute(route)
    }
    return busRoutesList
  }
  
  /*
   * Ritorna tutte le tratte (percorsi) del tram della mappa
   */
  def getAllTramRoutes() : List[tram_route] = {
    var maxRoute = -1
    for(tram_stop <- map.tram_stops) {
      if(tram_stop.route > maxRoute) {
        maxRoute = tram_stop.route
      }
    }
    var tramRoutesList = List[tram_route]()
    for(route <- 1 to maxRoute) {
      tramRoutesList = tramRoutesList :+ createTramRoute(route)
    }
    return tramRoutesList
  }
  
  /*
   * Dato il percorso in input e tutti i possibili pezzi di una tratta,
   * restituisce il più lungo pezzo di tratta che può essere rimpiazzato nel percorso in input
   * Restituisce inoltre gli indici di inzio e fine (inclusivi) corrispondenti nel percorso originale
   */
  def findLongestSubslice(route : List[step], slices : List[List[step]]) : (List[step],Int,Int) = {
    // per trovare il subslice, bisogna vedere se nel percorso in input ci sono l'inizio e la fine dello slice
    // attraversati nella stessa direzione
    // restituisce il tratto di tratta e gli indici di inizio e di fine nel percorso originale
    var index = -1
    var length = -1
    var startIndex = -1
    var endIndex = -1
    for(slice <- slices) {
      // prendi inzio e fine dello slice (con ignore = true)
      var begin = slice.head
      var end = slice.last
      slice.head match {
        case bus_stop_step(bus_stop, direction, _) =>
          begin = bus_stop_step(bus_stop, direction, true)
        case tram_stop_step(tram_stop, direction, _) =>
          begin = tram_stop_step(tram_stop, direction, true)
      }
      slice.last match {
        case bus_stop_step(bus_stop, direction, _) =>
          end = bus_stop_step(bus_stop, direction, true)
        case tram_stop_step(tram_stop, direction, _) =>
          end = tram_stop_step(tram_stop, direction, true)
      }
      // controlla se sono presenti con lo stesso ordine dentro al route in input
      val currStartIndex = route.indexOf(begin)
      val currEndIndex = route.indexOf(end)
      if(currStartIndex != -1 && currEndIndex != -1 && currStartIndex < currEndIndex && (currEndIndex - currStartIndex + 1) > length) {
        index = slices.indexOf(slice)
        startIndex = currStartIndex
        endIndex = currEndIndex
        length = endIndex - startIndex + 1
      }
    }
    if(index != -1) {
      return (slices(index), startIndex, endIndex)
    }
    else {
      return (List[step](), startIndex, endIndex)
    }
  }
  
  /*
   * Dato un percorso in input e tutti i possibili pezzi di tutte le tratte di mezzi pubblici,
   * restituisce il percorso alla quale è stato sostituito un pezzo in favore del più lungo tratto con mezzi pubblici
   * compatibile, qualora tale pezzo esista, o lascia inalterato il percorso altrimenti.
   */
  def routeReplacement(input : List[step], busRoutesSlices : List[List[List[step]]], tramRoutesSlices : List[List[List[step]]]) : List[step] = {
	  // modifica il percorso in input sulla base dei mezzi pubblici in input
    // il percorso viene modificato se esiste un pezzo del percorso che può essere eseguito con un mezzo pubblico
    //
    // modifica
    // si decide di preferire il trasporto via tram quando possibile
    // si effettua dunque il replacement con il tram
    // solo se questo non sortisce effetto, allora si passa al bus
    var subslice = List[step]()
    var startIndex = -1
    var endIndex = -1
	  for(tramRouteSlices <- tramRoutesSlices) {
		  val (longestSubslice, start, end) = findLongestSubslice(input, tramRouteSlices)
		  if(longestSubslice.length > subslice.length) {
			  subslice = longestSubslice
        startIndex = start
        endIndex = end
		  }
    }
	  // è stato trovato il tratto più lungo percorribile con mezzi pubblici
	  if(subslice.length != 0) {
		  // bisogna rimpiazzare il pezzo di percorso a piedi col mezzo pubblico
		  // lo step iniziale e quello finale del subslice sono ovviamente contenuti nel route in input,
		  // tranne per il fatto che sono ignorati
      // allora basta prendere il pezzo di percorso a piedi e sostituirlo con step iniziale e finale del subslice
      val substitution = List(subslice.head, subslice.last)
      return input.slice(0, startIndex) ++ substitution ++ input.slice(endIndex + 1, input.length)
	  }
    else {
      for(busRouteSlices <- busRoutesSlices) {
        val (longestSubslice, start, end) = findLongestSubslice(input, busRouteSlices)
        if(longestSubslice.length > subslice.length) {
          subslice = longestSubslice
          startIndex = start
          endIndex = end
        }
      }
      if(subslice.length != 0) {
        // bisogna rimpiazzare il pezzo di percorso a piedi col mezzo pubblico
        // lo step iniziale e quello finale del subslice sono ovviamente contenuti nel route in input,
        // tranne per il fatto che sono ignorati
        // allora basta prendere il pezzo di percorso a piedi e sostituirlo con step iniziale e finale del subslice
        val substitution = List(subslice.head, subslice.last)
        return input.slice(0, startIndex) ++ substitution ++ input.slice(endIndex + 1, input.length)
      }
      else {
        return input
      }
    }
  }
  
  /*
   * Funzione per la creazione di un percorso per pedone con utilizzo del tram forzoso
   * Sceglie le tre destinazioni (home, work e fun) casualmente e genera i percorsi per raggiungerli
   * Quando possibile, prende un mezzo pubblico (al massimo una volta per ogni pezzo, home->work, work->fun, fun->home)
   */
  def createPedestrianRouteWithTram() : (pedestrian_route, Boolean) = {
    // oltre al percorso, restituisce un flag
    // se il flag è a true, allora il percorso include l'utilizzo di un mezzo pubblico in uno dei tre pezzi
    // crea i tempi
    val times = createTimes()
    // crea le destinazioni
    val places = createPlaces()
    // decidi il pezzo di percorso su cui effettuare l'inserimento forzoso
    val index = nextInt(100) % 3
    return createPedestrianRouteWithTram(places, times, index)
  }
  
  def createPedestrianRouteWithTram(places : (String, String, String), times : (TimeValue, TimeValue, TimeValue), index : Int) : (pedestrian_route, Boolean) = {
    var firstRoute : List[step] = null
    var secondRoute : List[step] = null
    var thirdRoute : List[step] = null
    // ottieni due fermate del tram appartenenti alla stessa tratta
    val targetRoute = (nextInt(100) % getAllTramRoutes().length) + 1
    var tramStops = JSONReader.getAllTramStops(map).filter { tramStop => tramStop.route == targetRoute }
    assert(tramStops.length > 1)
    var firstTramStop : tram_stop = null
    var secondTramStop : tram_stop = null
    // decidi su quale pezzo di percorso effettuare l'inserimento forzoso
    if(index == 0) {
      // tra casa e lavoro
      firstTramStop = getClosestTramStop(places._1, tramStops)
      secondTramStop = getClosestTramStop(places._2, tramStops)
      if(firstTramStop.id == secondTramStop.id) {
        tramStops = tramStops.filter { tramStop => tramStop.id != firstTramStop.id }
        secondTramStop = getClosestTramStop(places._2, tramStops)
      }
      val firstSection = fromZoneToTramStop(map, places._1, firstTramStop.id)
      val previousDirection = getStepDirection(firstSection.last)
      val intermediateStep = tram_stop_step(firstTramStop, previousDirection, false)
      val secondSection = fromTramStopToZone(map, secondTramStop.id, places._2)
      firstRoute = (firstSection :+ intermediateStep) ++ secondSection
      secondRoute = pedestrianBreadthFirstSearch(map, places._2, places._3)
      thirdRoute = pedestrianBreadthFirstSearch(map, places._3, places._1)
    }
    else if(index == 1) {
      // tra lavoro e svago
      firstTramStop = getClosestTramStop(places._2, tramStops)
      secondTramStop = getClosestTramStop(places._3, tramStops)
      if(firstTramStop.id == secondTramStop.id) {
        tramStops = tramStops.filter { tramStop => tramStop.id != firstTramStop.id }
        secondTramStop = getClosestTramStop(places._3, tramStops)
      }
      firstRoute = pedestrianBreadthFirstSearch(map, places._1, places._2)
      val firstSection = fromZoneToTramStop(map, places._2, firstTramStop.id)
      val previousDirection = getStepDirection(firstSection.last)
      val intermediateStep = tram_stop_step(firstTramStop, previousDirection, false)
      val secondSection = fromTramStopToZone(map, secondTramStop.id, places._3)
      secondRoute = (firstSection :+ intermediateStep) ++ secondSection
      thirdRoute = pedestrianBreadthFirstSearch(map, places._3, places._1)
    }
    else {
      // tra svago e casa
      firstTramStop = getClosestTramStop(places._3, tramStops)
      secondTramStop = getClosestTramStop(places._1, tramStops)
      if(firstTramStop.id == secondTramStop.id) {
        tramStops = tramStops.filter { tramStop => tramStop.id != firstTramStop.id }
        secondTramStop = getClosestTramStop(places._1, tramStops)
      }
      firstRoute = pedestrianBreadthFirstSearch(map, places._1, places._2)
      secondRoute = pedestrianBreadthFirstSearch(map, places._2, places._3)
      val firstSection = fromZoneToTramStop(map, places._3, firstTramStop.id)
      val previousDirection = getStepDirection(firstSection.last)
      val intermediateStep = tram_stop_step(firstTramStop, previousDirection, false)
      val secondSection = fromTramStopToZone(map, secondTramStop.id, places._1)
      thirdRoute = (firstSection :+ intermediateStep) ++ secondSection
    }
    return (pedestrian_route(times._1, firstRoute, times._2, secondRoute, times._3, thirdRoute), true)
  }
  
  def getClosestTramStop(zoneId : String, tramStops : List[tram_stop]) : tram_stop = {
    val (_, zonex, zoney, _) = splitId(zoneId)
    val zonePoint = point(zonex, zoney)
    var distance = scala.Double.MaxValue
    var result : tram_stop = null
    for(tramStop <- tramStops) {
      if(getDistance(zonePoint, tramStop.coordinates.begin.point) < distance) {
        result = tramStop
        distance = getDistance(zonePoint, tramStop.coordinates.begin.point)
      }
    }
    return result
  }
  
}