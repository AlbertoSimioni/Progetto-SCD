package map

import scala.collection.mutable.Stack

import Domain._
import Domain.position._
import JSONUtilities._
import JSONReader._
import Routes._

object BreadthFirst {
  
  class WrongDirectionException extends Exception
  
  /*
   * Effettua una ricerca di tipo breadth-first in un grafo coerente con gli spostamenti pedonali.
   * Restituisce il percorso dall'inizio alla fine.
   * Essendo il grafo della mappa connesso, il percorso viene sempre trovato.
   * Per i pedoni, gli unici nodi del grafo sono gli attraversamenti pedonali.
   * Ulteriori due nodi sono il nodo zona di partenza e il nodo zona di arrivo.
   */
  def pedestrianBreadthFirstSearch(map : urban_elements, start : String, end : String) : List[step] = {
    
    // esco da un edificio e devo trovarne un altro
    // ogni step deve dichiarare:
    // 1) l'entità
    // 2) il lato di uscita (up, down, left, right)
    // 3) il verso da cui si sta uscendo
    // uscendo dall'edificio ho due strade a disposizione: prendo il marciapiede della strada nello stesso lato
    // di presenza dell'edificio e posso proseguire o concordemente alla strada (beginToEnd) o in verso opposto
    // (!beginToEnd). Questo corrsiponde ad un road_step con posizione concorde con quella dell'edificio e beginToEnd variabile.
    // A partire dal road_step, come si capisce chi si trova? Basta guardare il campo id del begin o dell'end, 
    // in base al verso di percorrenza della strada.
    // Se si trova un incrocio, bisogna capire qual'è il vertice corrispondente al lato di arrivo della strada.
    // Se si trovano delle strisce pedonali, si aprono due strade:
    // 1) ignorarle, dunque si prosegue con il vicino tenendo lo stesso verso di percorrenza
    // 2) usarle
    // Quando si usano le strisce, bisogna guardare il lato di percorrenza da dove si veniva:
    // 1) se è left o right, allora le strisce sono su di una strada verticale, dunque usandole si otterrà il lato opposto
    //    (right o left) e si deciderà se andare verso l'alto (vicino con y più alta) o verso il basso (vicino con y più bassa)
    // 2) se è up o down, allora le strisce sono su di una strada orizzontale, dunque usandole si otterrà il lato opposto
    //    (down o up) e si deciderà se andare verso destra (vicino con x più alta) o verso sinistra (vicino con x più bassa)
    // Se si trovano fermate del bus o del tram, per il momento le si ignorano
    // Se si percorre una strada, si controllano le zone con il medesimo lato di percorrenza (se è stata trovata la zona cercata)
    // 
    // Per quanto riguarda il grafo, qualunque entità stradale al di fuori delle strisce pedonali non costituisce un nodo in quanto
    // non consiste in una reale alternativa
    
    var whiteNodes = List[String]()
    var grayNodes = List[String]()
    var blackNodes = List[String]()
    // inizializzazione
    for(pedestrian_crossroad <- map.pedestrian_crossroads) {
      whiteNodes = whiteNodes :+ pedestrian_crossroad.id
    }
    whiteNodes = whiteNodes :+ start
    whiteNodes = whiteNodes :+ end
    // prepara la struttura per i padri
    // mappa con:
    // - ultimo elemento del percorso
    // - percorso per raggiungerlo (ultimo elemento escluso)
    var fatherRoutes = Map[String, List[step]]()
    // colora di grigio il nodo di partenza
    whiteNodes = whiteNodes.toSet.diff(Set(start)).toList
    grayNodes = grayNodes :+ start
    // aggiungi l'inizio alla pila
    var stack = new Stack[String]
    stack = stack.push(start)
    var exit = false
    while(stack.isEmpty == false && exit == false) {
      val id = stack.pop()
      var adjacencyList = Map[String, List[step]]()
      // capisco l'entità, che può essere una zona o un pedestrian crossroad
      val (entity, _, _, _) = splitId(id)
      entity match {
        case "pedestrian_crossroad" =>
          // sicuramente non stiamo partendo da qui
          assert(fatherRoutes.size > 0)
          // recupera la direzione dell'ultimo step
          val prevDirection = getStepDirection(fatherRoutes.get(id).get.last)
          // crea il percorso fino alle prossime zone o alle prossime strisce pedonali
          // tre percorsi possibili
          // 1) ignorando le strisce
          adjacencyList = adjacencyList +  createPedestrianPartialRoute(map, id, prevDirection, end)
          // 2) attraversandole, con le due possibili diramazioni
          adjacencyList = adjacencyList + createPedestrianPartialRoute(map, id, direction(getOpposite(prevDirection.position), prevDirection.beginToEnd), end)
          adjacencyList = adjacencyList + createPedestrianPartialRoute(map, id, direction(getOpposite(prevDirection.position), !prevDirection.beginToEnd), end)
        case "zone" =>
          if(start == id) {
            // siamo allo step iniziale
            // crea i due percorsi (dx-sx oppure up-down) fino alle prossime zone o alle prossime strisce pedonali
            val zone = getZone(map, id).get
            adjacencyList = adjacencyList + createPedestrianPartialRoute(map, id, direction(zone.position, true), end)
            adjacencyList = adjacencyList + createPedestrianPartialRoute(map, id, direction(zone.position, false), end)
          }
          else {
            // siamo allo step finale
            assert(end == id)
            // puoi uscire dal while
            exit = true
          }
      }
      // per ciascuna delle destinazioni
      for((end, partialRoute) <- adjacencyList) {
        // se la destinazione è bianca
        if(whiteNodes.contains(end) == true) {
          // rendila grigia
          val size = whiteNodes.size
          whiteNodes = whiteNodes.toSet.diff(Set(end)).toList
          assert(size != whiteNodes.size)
          grayNodes = grayNodes :+ end
          // aggiungi alla map il percorso parziale
          val tuple = (end, partialRoute)
          fatherRoutes = fatherRoutes + tuple
          // aggiungi la destinazione alla pila
          stack = stack.push(end)
        }
      }
      // colora di nero la zona attuale
      grayNodes = grayNodes.toSet.diff(Set(id)).toList
      blackNodes = blackNodes :+ id
    }
    // possiamo costruire a ritroso il percorso finale
    var id = end
    var finalRoute = List[step]()
    while(id != start) {
      val partialRoute = fatherRoutes(id)
      finalRoute = partialRoute ++ finalRoute
      id = getStepId(partialRoute.head)
    }
    return finalRoute
  }
  
  /*
   * Effettua una ricerca di tipo breadth-first in un grafo coerente con gli spostamenti in automobile.
   * Restituisce il percorso dall'inizio alla fine.
   * Essendo il grafo della mappa connesso, il percorso viene sempre trovato.
   * Per le macchine, gli unici nodi del grafo sono gli incroci.
   * Ulteriori due nodi sono il nodo zona di partenza e il nodo zona di arrivo.
   */
  def carBreadthFirstSearch(map : urban_elements, start : String, end : String) : List[step] = {
    
    // esco da un edificio e devo trovarne un altro
    // ogni step deve dichiarare:
    // 1) l'entità
    // 2) il lato di uscita (up, down, left, right)
    // 3) il verso da cui si sta uscendo
    // uscendo da un edificio, posso prendere le due corsie della strada, in base alla direzione
    // se incontro un incrocio, posso finire in uno qualunque dei vertici
    // se incontro una fermata del bus o del tram, la ignoro
    // se incontro un attraversamento pedonale, lo ignoro
    // se incontro una zona, dipende (o la ignoro o mi ci fermo)
    
    var whiteNodes = List[String]()
    var grayNodes = List[String]()
    var blackNodes = List[String]()
    // inizializzazione
    for(crossroad <- map.crossroads) {
      whiteNodes = whiteNodes :+ crossroad.id
    }
    whiteNodes = whiteNodes :+ start
    whiteNodes = whiteNodes :+ end
    // prepara la struttura per i padri
    // mappa con:
    // - ultimo elemento del percorso
    // - percorso per raggiungerlo (ultimo elemento escluso)
    var fatherRoutes = Map[String, List[step]]()
    // colora di grigio il nodo di partenza
    whiteNodes = whiteNodes.toSet.diff(Set(start)).toList
    grayNodes = grayNodes :+ start
    // aggiungi l'inizio alla pila
    var stack = new Stack[String]
    stack = stack.push(start)
    var exit = false
    while(stack.isEmpty == false && exit == false) {
      val id = stack.pop()
      var adjacencyList = Map[String, List[step]]()
      // capisco l'entità, che può essere una zona o un crossroad
      val (entity, _, _, _) = splitId(id)
      entity match {
        case "crossroad" =>
          // sicuramente non stiamo partendo da qui
          assert(fatherRoutes.size > 0)
          // recupera la direzione dell'ultimo step
          val prevDirection = getStepDirection(fatherRoutes.get(id).get.last)
          // bisogna creare i percorsi fino alle prossime zone o ai prossimi incroci
          // data la direzione di arrivo, quante direzioni possibili?
          // Se il vertice ha tipo nil, una sola, identica a quella di arrivo
          // Altrimenti, una per ogni vertice (se il vertice non ha id nil)
          val directions = getCrossroadAvailableDirections(getCrossroad(map, id).get, prevDirection)
          for(direction <- directions) {
            adjacencyList = adjacencyList + createCarPartialRoute(map, id, direction, end)
          }
        case "zone" =>
          if(start == id) {
            // siamo allo step iniziale
            // crea i due percorsi (dx-sx oppure up-down) fino al prossimo incrocio
            val zone = getZone(map, id).get
            zone.position match {
              case position.up =>
                adjacencyList = adjacencyList + createCarPartialRoute(map, id, direction(getOpposite(zone.position), true), end)
                adjacencyList = adjacencyList + createCarPartialRoute(map, id, direction(zone.position, false), end)
              case position.down =>
                adjacencyList = adjacencyList + createCarPartialRoute(map, id, direction(zone.position, true), end)
                adjacencyList = adjacencyList + createCarPartialRoute(map, id, direction(getOpposite(zone.position), false), end)
              case position.left =>
                adjacencyList = adjacencyList + createCarPartialRoute(map, id, direction(getOpposite(zone.position), true), end)
                adjacencyList = adjacencyList + createCarPartialRoute(map, id, direction(zone.position, false), end)
              case position.right =>
                adjacencyList = adjacencyList + createCarPartialRoute(map, id, direction(zone.position, true), end)
                adjacencyList = adjacencyList + createCarPartialRoute(map, id, direction(getOpposite(zone.position), false), end)
            }
          }
          else {
            // siamo allo step finale
            assert(end == id)
            // puoi uscire dal while
            exit = true
          }
      }
      // per ciascuna delle destinazioni
      for((end, partialRoute) <- adjacencyList) {
        // se la destinazione è bianca
        if(whiteNodes.contains(end) == true) {
          // rendila grigia
          whiteNodes = whiteNodes.toSet.diff(Set(end)).toList
          grayNodes = grayNodes :+ end
          // aggiungi alla map il percorso parziale
          val tuple = (end, partialRoute)
          fatherRoutes = fatherRoutes + tuple
          // aggiungi la destinazione alla pila
          stack = stack.push(end)
        }
      }
      // colora di nero la zona attuale
      grayNodes = grayNodes.toSet.diff(Set(id)).toList
      blackNodes = blackNodes :+ id
    }
    // possiamo costruire a ritroso il percorso finale
    var id = end
    var finalRoute = List[step]()
    while(id != start) {
      val partialRoute = fatherRoutes(id)
      finalRoute = partialRoute ++ finalRoute
      id = getStepId(partialRoute.head)
    }
    return finalRoute
  }
  
  /*
   * Crea un arco del grafo per pedoni. Parte da una zona (inizio) o da un attraversamento pedonale
   * e si ferma in una zona (fine) o in un altro attraversamento pedonale.
   * Target è l'identificativo della zona a cui bisogna arrivare (qualora venga trovata, si può restituire il percorso)
   */
  def createPedestrianPartialRoute(map : urban_elements, startEntity : String, direction : direction, target : String) : (String, List[step]) = {
    var steps = List[step]()
    var neighbor = ""
    // la startEntity può essere una zone o un pedestrian_crossroad
    if(getZone(map, startEntity).equals(None)) {
      // pedestrian_crossroad
      val pedestrian_crossroad = getPedestrianCrossroad(map, startEntity).get
      // crea lo step
      steps = steps :+ pedestrian_crossroad_step(pedestrian_crossroad, direction)
      // in base al verso di percorrenza si decide il vicino
      if(direction.beginToEnd == true) {
        // bisogna prendere il vicino su end
        neighbor = pedestrian_crossroad.coordinates.end.id
      }
      else {
        // bisogna prendere il vicino su begin
        neighbor = pedestrian_crossroad.coordinates.begin.id
      }
    }
    else {
      // zone
      val zone = getZone(map, startEntity).get
      // il lato di percorrenza della direzione è concorde con quello della zona
      assert(direction.position == zone.position)
      // crea lo step
      steps = steps :+ zone_step(zone, direction)
      /*
      // in base al verso di percorrenza si decide il vicino
      if(direction.beginToEnd == true) {
        // bisogna prendere il vicino su end
        neighbor = zone.coordinates.end.id
      }
      else {
        // bisogna prendere il vicino su begin
        neighbor = zone.coordinates.begin.id
      }
      */
      // a prescindere dal verso di percorrenza, un zone step DEVE essere seguito da un road step relativo alla road di appartenenza
      neighbor = zone.road
    }
    var finished = false
    while(finished == false) {
      val (entity, x, y, z) = splitId(neighbor)
      // recupera l'id dello step precedente
      val previousId = getStepId(steps.last)
      // recupera la direction dello step precedente
      val previousDirection = getStepDirection(steps.last)
      entity match {
        case "road" =>
          // si crea lo step della strada con la stessa direzione precedente
          val road = getRoad(map, neighbor).get
          steps = steps :+ road_step(road, previousDirection)
          if(road.zonesIDs.contains(target) == true && previousDirection.position == getZone(map, target).get.position) {
            // è stata trovata la fine del percorso
            neighbor = target
          }
          // per decretare il vicino, basta guardare il beginToEnd della previousDirection
          else if(previousDirection.beginToEnd == true) {
            // restituisco il vicino end
            neighbor = road.coordinates.end.id
          }
          else {
            // restituisco il vicino begin
            neighbor = road.coordinates.begin.id
          }
        case "crossroad" =>
          // si applicano le regole per creare il nuovo step
          val crossroad = getCrossroad(map, neighbor).get
          var availableVertexes = crossroad.vertexes.filter { vertex => vertex.id != previousId && vertex.id != "nil"}
          // se la tipologia dell'incrocio è nil, allora ignoralo
          if(crossroad.category == category.nil) {
            // crea lo step in cui la direction è invariata
            steps = steps :+ crossroad_step(crossroad, previousDirection)
            // trova il vertice concorde alla direzione
            val previousVertex = getCorrespondingVertex(previousId, map, crossroad)
            availableVertexes = availableVertexes.toSet.diff(Set(previousVertex)).toList
            val (id, _) = getPedestrianCrossroadNeighbor(previousVertex, previousDirection, availableVertexes)
            neighbor = id
          }
          else {
            val previousVertex = getCorrespondingVertex(previousId, map, crossroad)
            availableVertexes = availableVertexes.toSet.diff(Set(previousVertex)).toList
            val (id, direction) = getPedestrianCrossroadNeighbor(previousVertex, previousDirection, availableVertexes)
            steps = steps :+ crossroad_step(crossroad, direction)
            neighbor = id
          }
        case "pedestrian_crossroad" =>
          // il percorso parziale termina qui
          // non include l'ultima entità
          finished = true
        case "bus_stop" =>
          // il bus stop viene ignorato momentaneamente
          val bus_stop = getBusStop(map, neighbor).get
          steps = steps :+ bus_stop_step(bus_stop, previousDirection, true)
          // per decretare il vicino, basta guardare il beginToEnd della previousDirection
          if(previousDirection.beginToEnd == true) {
            // restituisco il vicino end
            neighbor = bus_stop.coordinates.end.id
          }
          else {
            // restituisco il vicino begin
            neighbor = bus_stop.coordinates.begin.id
          }
        case "tram_stop" =>
          // il tram stop viene ignorato momentaneamente
          val tram_stop = getTramStop(map, neighbor).get
          steps = steps :+ tram_stop_step(tram_stop, previousDirection, true)
          // per decretare il vicino, basta guardare il beginToEnd della previousDirection
          if(previousDirection.beginToEnd == true) {
            // restituisco il vicino end
            neighbor = tram_stop.coordinates.end.id
          }
          else {
            // restituisco il vicino begin
            neighbor = tram_stop.coordinates.begin.id
          }
        case "zone" =>
          // il percorso parziale termina qui
          // non include l'ultima entità
          finished = true
      }
    }
    // neighbor contiene l'id dell'ultima entità, che è una pedestrian_crossroad o una zone
    return (neighbor, steps)
  }
  
  /*
   * Crea un arco del grafo per automobili. Parte da una zona (inizio) o da un incrocio
   * e si ferma in una zona (fine) o in un altro incrocio.
   * Target è l'identificativo della zona a cui bisogna arrivare (qualora venga trovata, si può restituire il percorso)
   */
  def createCarPartialRoute(map : urban_elements, startEntity : String, direction : direction, target : String) : (String, List[step]) = {
    var steps = List[step]()
    var neighbor = ""
    // la startEntity può essere una zone o un crossroad
    if(getZone(map, startEntity).equals(None)) {
      // crossroad
      val crossroad = getCrossroad(map, startEntity).get
      // crea lo step
      steps = steps :+ crossroad_step(crossroad, direction)
      // in base al verso di percorrenza si decide il vicino
      neighbor = getCarCrossroadCorrespondingNeighbor(crossroad, direction)
    }
    else {
      // zone
      val zone = getZone(map, startEntity).get
      // crea lo step
      steps = steps :+ zone_step(zone, direction)
      /*
      // in base al verso di percorrenza si decide il vicino
      if(direction.beginToEnd == true) {
        // bisogna prendere il vicino su end
        neighbor = zone.coordinates.end.id
      }
      else {
        // bisogna prendere il vicino su begin
        neighbor = zone.coordinates.begin.id
      }
      */
      // a prescindere dal verso di percorrenza, si deve sempre avere un lane step
      // ci è sufficiente indicare la road come vicino, poi la gestione della road troverà la lane opportuna
      neighbor = zone.road
    }
    var finished = false
    while(finished == false) {
      val (entity, x, y, z) = splitId(neighbor)
      // recupera l'id dello step precedente
      val previousId = getStepId(steps.last)
      // recupera la direction dello step precedente
      val previousDirection = getStepDirection(steps.last)
      entity match {
        case "road" =>
          // si crea un lane step in base alla direzione precedente
          val road = getRoad(map, neighbor).get
          val lane = road.lanesIDs.filter { lane => getLane(map, lane).get.begintoend == previousDirection.beginToEnd && getLane(map, lane).get.tram == false}.head
          steps = steps :+ lane_step(getLane(map, lane).get, previousDirection)
          if(road.zonesIDs.contains(target) == true) {
            // è stata trovata la fine del percorso
            neighbor = target
          }
          // per decretare il vicino, basta guardare il beginToEnd della previousDirection
          else if(previousDirection.beginToEnd == true) {
            // restituisco il vicino end
            neighbor = road.coordinates.end.id
          }
          else {
            // restituisco il vicino begin
            neighbor = road.coordinates.begin.id
          }
        case "crossroad" =>
          // il percorso parziale termina qui
          // non include l'ultima entità
          finished = true
        case "pedestrian_crossroad" =>
          // l'attraversamento pedonale viene ignorato
          val pedestrian_crossroad = getPedestrianCrossroad(map, neighbor).get
          steps = steps :+ pedestrian_crossroad_step(pedestrian_crossroad, previousDirection)
          // per decretare il vicino, basta guardare il beginToEnd della previousDirection
          if(previousDirection.beginToEnd == true) {
            // restituisco il vicino end
            neighbor = pedestrian_crossroad.coordinates.end.id
          }
          else {
            // restituisco il vicino begin
            neighbor = pedestrian_crossroad.coordinates.begin.id
          }
        case "bus_stop" =>
          // il bus stop viene ignorato
          val bus_stop = getBusStop(map, neighbor).get
          steps = steps :+ bus_stop_step(bus_stop, previousDirection, true)
          // per decretare il vicino, basta guardare il beginToEnd della previousDirection
          if(previousDirection.beginToEnd == true) {
            // restituisco il vicino end
            neighbor = bus_stop.coordinates.end.id
          }
          else {
            // restituisco il vicino begin
            neighbor = bus_stop.coordinates.begin.id
          }
        case "tram_stop" =>
          // il tram stop viene ignorato
          val tram_stop = getTramStop(map, neighbor).get
          steps = steps :+ tram_stop_step(tram_stop, previousDirection, true)
          // per decretare il vicino, basta guardare il beginToEnd della previousDirection
          if(previousDirection.beginToEnd == true) {
            // restituisco il vicino end
            neighbor = tram_stop.coordinates.end.id
          }
          else {
            // restituisco il vicino begin
            neighbor = tram_stop.coordinates.begin.id
          }
        case "zone" =>
          // il percorso parziale termina qui
          // non include l'ultima entità
          finished = true
      }
    }
    // neighbor contiene l'id dell'ultima entità, che è un crossroad o una zone
    return (neighbor, steps)
  }
  
  /*
   * Funzione di utilità
   * Dato un id e un incrocio, restituisce il vertice corrispondente nell'incrocio.
   */
  def getCorrespondingVertex(id : String, map : urban_elements, crossroad : crossroad) : vertex = {
    // normalmente, l'id è già contenuto nei vertici
    // nel caso in cui l'id sia di una zona, nel vertice vi sarà la strada di appartenenza
    val filtered = crossroad.vertexes.filter { vertex => vertex.id == id }
    // o non c'è nessuno, o al massimo ce n'è uno
    assert(filtered.length <= 1)
    if(filtered.length == 0) {
      // siamo nel caso di una zona
      // recuperiamo la strada di appartenenza e cerchiamo quella
      val zone = getZone(map, id).get
      val newFiltered = crossroad.vertexes.filter { vertex => vertex.id == zone.road }
      // deve esserci uno e un solo elemento corrispondente
      assert(newFiltered.length == 1)
      return newFiltered(0)
    }
    else {
      return filtered(0)
    }
  }
  
  /*
   * Data una lista di vertici, il vertice entrante e la direzione di ingresso, restituisce
   * il vertice di uscita (id) e la direzione di uscita
   */
  def getPedestrianCrossroadNeighbor(comingVertex : vertex, comingDirection : direction, vertexes : List[vertex]) : (String, direction) = {
    if(comingDirection.beginToEnd == true) {
      comingDirection.position match {
        case position.right =>
          // Verso alto, dx => vertice più a dx e in alto
          //                => se x del vertice è invariata, il lato di percorrenza rimane identico (right)
          //                => se x del vertice aumenta, il lato di percorrenza diventa down
          //                => se x del vertice diminuisce, il lato di percorrenza diventa up
          var newVertex = vertex(point(0, 0), "")
          var maxX = -1
          var maxY = -1
          for(vertex <- vertexes) {
            if(vertex.point.x > maxX || (vertex.point.x == maxX && vertex.point.y > maxY)) {
              maxX = vertex.point.x
              maxY = vertex.point.y
              newVertex = vertex
            }
          }
          if(comingVertex.point.x == newVertex.point.x) {
            return (newVertex.id, comingDirection)
          }
          else if(comingVertex.point.x < newVertex.point.x) {
            return(newVertex.id, direction(position.down, true))
          }
          else {
            return(newVertex.id, direction(position.up, false))
          }
        case position.left =>
          // Verso alto, sx => vertice più a sx e in alto
          //                => se x del vertice è invariata, il lato di percorrenza rimane identico (left)
          //                => se x del vertice aumenta, il lato di percorrenza diventa up
          //                => se x del vertice diminuisce, il lato di percorrenza diventa down
          var newVertex = vertex(point(0, 0), "")
          var minX = scala.Int.MaxValue
          var maxY = -1
          for(vertex <- vertexes) {
            if(vertex.point.x < minX || (vertex.point.x == minX && vertex.point.y > maxY)) {
              minX = vertex.point.x
              maxY = vertex.point.y
              newVertex = vertex
            }
          }
          if(comingVertex.point.x == newVertex.point.x) {
            return (newVertex.id, comingDirection)
          }
          else if(comingVertex.point.x < newVertex.point.x) {
            return(newVertex.id, direction(position.up, true))
          }
          else {
            return(newVertex.id, direction(position.down, false))
          }
        case position.up =>
          // Verso dx, up => vertice più in alto e a dx
          //                => se y del vertice è invariata, il lato di percorrenza rimane identico (up)
          //                => se y del vertice aumenta, il lato di percorrenza diventa left
          //                => se y del vertice diminuisce, il lato di percorrenza diventa right
          var newVertex = vertex(point(0, 0), "")
          var maxX = -1
          var maxY = -1
          for(vertex <- vertexes) {
            if(vertex.point.y > maxY || (vertex.point.y == maxY && vertex.point.x > maxX)) {
              maxX = vertex.point.x
              maxY = vertex.point.y
              newVertex = vertex
            }
          }
          if(comingVertex.point.y == newVertex.point.y) {
            return (newVertex.id, comingDirection)
          }
          else if(comingVertex.point.y < newVertex.point.y) {
            return(newVertex.id, direction(position.left, true))
          }
          else {
            return(newVertex.id, direction(position.right, false))
          }
        case position.down =>
          // Verso dx, down => vertice più in basso e a dx
          //                => se y del vertice è invariata, il lato di percorrenza rimane identico (down)
          //                => se y del vertice aumenta, il lato di percorrenza diventa right
          //                => se y del vertice diminuisce, il lato di percorrenza diventa left
          var newVertex = vertex(point(0, 0), "")
          var maxX = -1
          var minY = scala.Int.MaxValue
          for(vertex <- vertexes) {
            if(vertex.point.y < minY || (vertex.point.y == minY && vertex.point.x > maxX)) {
              maxX = vertex.point.x
              minY = vertex.point.y
              newVertex = vertex
            }
          }
          if(comingVertex.point.y == newVertex.point.y) {
            return (newVertex.id, comingDirection)
          }
          else if(comingVertex.point.y < newVertex.point.y) {
            return(newVertex.id, direction(position.right, true))
          }
          else {
            return(newVertex.id, direction(position.left, false))
          }
      }
    }
    else {
      comingDirection.position match {
        case position.right =>
          // Verso basso, dx => vertice più a dx e in basso
          //                => se x del vertice è invariata, il lato di percorrenza rimane identico (right)
          //                => se x del vertice aumenta, il lato di percorrenza diventa up
          //                => se x del vertice diminuisce, il lato di percorrenza diventa down
          var newVertex = vertex(point(0, 0), "")
          var maxX = -1
          var minY = scala.Int.MaxValue
          for(vertex <- vertexes) {
            if(vertex.point.x > maxX || (vertex.point.x == maxX && vertex.point.y < minY)) {
              maxX = vertex.point.x
              minY = vertex.point.y
              newVertex = vertex
            }
          }
          if(comingVertex.point.x == newVertex.point.x) {
            return (newVertex.id, comingDirection)
          }
          else if(comingVertex.point.x < newVertex.point.x) {
            return(newVertex.id, direction(position.up, true))
          }
          else {
            return(newVertex.id, direction(position.down, false))
          }
        case position.left =>
          // Verso basso, sx => vertice più a sx e in basso
          //                => se x del vertice è invariata, il lato di percorrenza rimane identico (left)
          //                => se x del vertice aumenta, il lato di percorrenza diventa down
          //                => se x del vertice diminuisce, il lato di percorrenza diventa up
          var newVertex = vertex(point(0, 0), "")
          var minX = scala.Int.MaxValue
          var minY = scala.Int.MaxValue
          for(vertex <- vertexes) {
            if(vertex.point.x < minX || (vertex.point.x == minX && vertex.point.y < minY)) {
              minX = vertex.point.x
              minY = vertex.point.y
              newVertex = vertex
            }
          }
          if(comingVertex.point.x == newVertex.point.x) {
            return (newVertex.id, comingDirection)
          }
          else if(comingVertex.point.x < newVertex.point.x) {
            return(newVertex.id, direction(position.down, true))
          }
          else {
            return(newVertex.id, direction(position.up, false))
          }
        case position.up =>
          // Verso sx, up => vertice più in alto e a sx
          //                => se y del vertice è invariata, il lato di percorrenza rimane identico (up)
          //                => se y del vertice aumenta, il lato di percorrenza diventa right
          //                => se y del vertice diminuisce, il lato di percorrenza diventa left
          var newVertex = vertex(point(0, 0), "")
          var minX = scala.Int.MaxValue
          var maxY = -1
          for(vertex <- vertexes) {
            if(vertex.point.y > maxY || (vertex.point.y == maxY && vertex.point.x < minX)) {
              minX = vertex.point.x
              maxY = vertex.point.y
              newVertex = vertex
            }
          }
          if(comingVertex.point.y == newVertex.point.y) {
            return (newVertex.id, comingDirection)
          }
          else if(comingVertex.point.y < newVertex.point.y) {
            return(newVertex.id, direction(position.right, true))
          }
          else {
            return(newVertex.id, direction(position.left, false))
          }
        case position.down =>
          // Verso sx, down => vertice più in basso e a sx
          //                => se y del vertice è invariata, il lato di percorrenza rimane identico (down)
          //                => se y del vertice aumenta, il lato di percorrenza diventa left
          //                => se y del vertice diminuisce, il lato di percorrenza diventa right
          var newVertex = vertex(point(0, 0), "")
          var minX = scala.Int.MaxValue
          var minY = scala.Int.MaxValue
          for(vertex <- vertexes) {
            if(vertex.point.y < minY || (vertex.point.y == minY && vertex.point.x < minX)) {
              minX = vertex.point.x
              minY = vertex.point.y
              newVertex = vertex
            }
          }
          if(comingVertex.point.y == newVertex.point.y) {
            return (newVertex.id, comingDirection)
          }
          else if(comingVertex.point.y < newVertex.point.y) {
            return(newVertex.id, direction(position.left, true))
          }
          else {
            return(newVertex.id, direction(position.right, false))
          }
      }
    }
  }
  
  /*
   * Dato un incrocio e la direzione di uscita, restituisce l'id del vertice corrispondente.
   * La logica è coerente con uno spostamento in automobile.
   */
  def getCarCrossroadCorrespondingNeighbor(crossroad : crossroad, direction : direction) : String = {
    // rimuovi vertici nil
    var availableVertexes = crossroad.vertexes.filter { vertex => vertex.id != "nil" }
    if(direction.beginToEnd == true) {
      direction.position match {
        case position.down =>
          // vertice più a destra (non dovrebbero esserci ambiguità vista la direzione di uscita)
          val vertex = availableVertexes.sortBy { vertex => vertex.point.x }.last
          return vertex.id
        case position.right =>
          // vertice più in alto (non dovrebbero esserci ambiguità vista la direzione di uscita)
          val vertex = availableVertexes.sortBy { vertex => vertex.point.y }.last
          return vertex.id
        case position.up | position.left =>
          // caso non possibile, imboccata corsia inversa
          throw new WrongDirectionException
      }
    }
    else {
      direction.position match {
        case position.up =>
          // vertice più a sinistra
          val vertex = availableVertexes.sortBy { vertex => vertex.point.x }.head
          return vertex.id
        case position.left =>
          // vertice più in basso
          val vertex = availableVertexes.sortBy { vertex => vertex.point.y }.head
          return vertex.id
        case position.down | position.right =>
          // caso non possibile, imboccata corsia inversa
          throw new WrongDirectionException
      }
    }
  }
  
  /*
   * Funzione di utilità per le automobili.
   * Dato un incrocio e la direzione di arrivo, restituisce la lista delle direzioni disponibili in uscita.
   */
  def getCrossroadAvailableDirections(crossroad : crossroad, arriving_direction : direction) : List[direction] = {
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
    var directions = List[direction]()
    if(crossroad.category == category.nil) {
      directions = directions :+ arriving_direction
    }
    else {
      for(vertex <- crossroad.vertexes.filter { current => current.id != arriving_vertex.id || (current.point.x != arriving_vertex.point.x || current.point.y != arriving_vertex.point.y) }) {
        if(vertex.id != "nil") {
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
              if(vertex.point.y == arriving_vertex.point.y) {
                directions = directions :+ arriving_direction
              }
              else if(vertex.point.y < arriving_vertex.point.y) {
                directions = directions :+ direction(position.left, false)
              }
              else {
                directions = directions :+ direction(position.right, true)
              }
            case position.left | position.right =>
              if(vertex.point.x == arriving_vertex.point.x) {
                directions = directions :+ arriving_direction
              }
              else if(vertex.point.x < arriving_vertex.point.x) {
                directions = directions :+ direction(position.up, false)
              }
              else {
                directions = directions :+ direction(position.down, true)
              }
          }
        }
      }
    }
    return directions
  }
  
  def fromZoneToTramStop(map : urban_elements, start : String, end : String) : List[step] = {
    var whiteNodes = List[String]()
    var grayNodes = List[String]()
    var blackNodes = List[String]()
    // inizializzazione
    for(pedestrian_crossroad <- map.pedestrian_crossroads) {
      whiteNodes = whiteNodes :+ pedestrian_crossroad.id
    }
    whiteNodes = whiteNodes :+ start
    whiteNodes = whiteNodes :+ end
    // prepara la struttura per i padri
    // mappa con:
    // - ultimo elemento del percorso
    // - percorso per raggiungerlo (ultimo elemento escluso)
    var fatherRoutes = Map[String, List[step]]()
    // colora di grigio il nodo di partenza
    whiteNodes = whiteNodes.toSet.diff(Set(start)).toList
    grayNodes = grayNodes :+ start
    // aggiungi l'inizio alla pila
    var stack = new Stack[String]
    stack = stack.push(start)
    var exit = false
    while(stack.isEmpty == false && exit == false) {
      val id = stack.pop()
      var adjacencyList = Map[String, List[step]]()
      // capisco l'entità, che può essere un pedestrian crossroad, una zone o un tram stop
      val (entity, _, _, _) = splitId(id)
      entity match {
        case "pedestrian_crossroad" =>
          // sicuramente non stiamo partendo da qui
          assert(fatherRoutes.size > 0)
          // recupera la direzione dell'ultimo step
          val prevDirection = getStepDirection(fatherRoutes.get(id).get.last)
          // crea il percorso fino alle prossime zone o alle prossime strisce pedonali
          // tre percorsi possibili
          // 1) ignorando le strisce
          adjacencyList = adjacencyList +  createPedestrianPartialRouteToTramStop(map, id, prevDirection, end)
          // 2) attraversandole, con le due possibili diramazioni
          adjacencyList = adjacencyList + createPedestrianPartialRouteToTramStop(map, id, direction(getOpposite(prevDirection.position), prevDirection.beginToEnd), end)
          adjacencyList = adjacencyList + createPedestrianPartialRouteToTramStop(map, id, direction(getOpposite(prevDirection.position), !prevDirection.beginToEnd), end)
        case "zone" =>
          // dobbiamo essere allo step iniziale
          assert(start == id)
          // siamo allo step iniziale
          // crea i due percorsi (dx-sx oppure up-down) fino alle prossime zone o alle prossime strisce pedonali
          val zone = getZone(map, id).get
          adjacencyList = adjacencyList + createPedestrianPartialRouteToTramStop(map, id, direction(zone.position, true), end)
          adjacencyList = adjacencyList + createPedestrianPartialRouteToTramStop(map, id, direction(zone.position, false), end)
        case "tram_stop" =>
          // siamo allo step finale
          assert(end == id)
          // puoi uscire dal while
          exit = true
      }
      // per ciascuna delle destinazioni
      for((end, partialRoute) <- adjacencyList) {
        // se la destinazione è bianca
        if(whiteNodes.contains(end) == true) {
          // rendila grigia
          val size = whiteNodes.size
          whiteNodes = whiteNodes.toSet.diff(Set(end)).toList
          assert(size != whiteNodes.size)
          grayNodes = grayNodes :+ end
          // aggiungi alla map il percorso parziale
          val tuple = (end, partialRoute)
          fatherRoutes = fatherRoutes + tuple
          // aggiungi la destinazione alla pila
          stack = stack.push(end)
        }
      }
      // colora di nero la zona attuale
      grayNodes = grayNodes.toSet.diff(Set(id)).toList
      blackNodes = blackNodes :+ id
    }
    // possiamo costruire a ritroso il percorso finale
    var id = end
    var finalRoute = List[step]()
    while(id != start) {
      val partialRoute = fatherRoutes(id)
      finalRoute = partialRoute ++ finalRoute
      id = getStepId(partialRoute.head)
    }
    return finalRoute
  }
  
  def fromTramStopToZone(map : urban_elements, start : String, end : String) : List[step] = {
    var whiteNodes = List[String]()
    var grayNodes = List[String]()
    var blackNodes = List[String]()
    // inizializzazione
    for(pedestrian_crossroad <- map.pedestrian_crossroads) {
      whiteNodes = whiteNodes :+ pedestrian_crossroad.id
    }
    whiteNodes = whiteNodes :+ start
    whiteNodes = whiteNodes :+ end
    // prepara la struttura per i padri
    // mappa con:
    // - ultimo elemento del percorso
    // - percorso per raggiungerlo (ultimo elemento escluso)
    var fatherRoutes = Map[String, List[step]]()
    // colora di grigio il nodo di partenza
    whiteNodes = whiteNodes.toSet.diff(Set(start)).toList
    grayNodes = grayNodes :+ start
    // aggiungi l'inizio alla pila
    var stack = new Stack[String]
    stack = stack.push(start)
    var exit = false
    while(stack.isEmpty == false && exit == false) {
      val id = stack.pop()
      var adjacencyList = Map[String, List[step]]()
      // capisco l'entità, che può essere un pedestrian crossroad, una zone o un tram stop
      val (entity, _, _, _) = splitId(id)
      entity match {
        case "pedestrian_crossroad" =>
          // sicuramente non stiamo partendo da qui
          assert(fatherRoutes.size > 0)
          // recupera la direzione dell'ultimo step
          val prevDirection = getStepDirection(fatherRoutes.get(id).get.last)
          // crea il percorso fino alle prossime zone o alle prossime strisce pedonali
          // tre percorsi possibili
          // 1) ignorando le strisce
          adjacencyList = adjacencyList +  createPedestrianPartialRouteFromTramStop(map, id, prevDirection, end)
          // 2) attraversandole, con le due possibili diramazioni
          adjacencyList = adjacencyList + createPedestrianPartialRouteFromTramStop(map, id, direction(getOpposite(prevDirection.position), prevDirection.beginToEnd), end)
          adjacencyList = adjacencyList + createPedestrianPartialRouteFromTramStop(map, id, direction(getOpposite(prevDirection.position), !prevDirection.beginToEnd), end)
        case "tram_stop" =>
          // dobbiamo essere allo step iniziale
          assert(start == id)
          // siamo allo step iniziale
          // crea i due percorsi (dx-sx oppure up-down) fino alle prossime zone o alle prossime strisce pedonali
          val tramStop = getTramStop(map, id).get
          adjacencyList = adjacencyList + createPedestrianPartialRouteFromTramStop(map, id, direction(tramStop.position, true), end)
          adjacencyList = adjacencyList + createPedestrianPartialRouteFromTramStop(map, id, direction(tramStop.position, false), end)
        case "zone" =>
          // siamo allo step finale
          assert(end == id)
          // puoi uscire dal while
          exit = true
      }
      // per ciascuna delle destinazioni
      for((end, partialRoute) <- adjacencyList) {
        // se la destinazione è bianca
        if(whiteNodes.contains(end) == true) {
          // rendila grigia
          val size = whiteNodes.size
          whiteNodes = whiteNodes.toSet.diff(Set(end)).toList
          assert(size != whiteNodes.size)
          grayNodes = grayNodes :+ end
          // aggiungi alla map il percorso parziale
          val tuple = (end, partialRoute)
          fatherRoutes = fatherRoutes + tuple
          // aggiungi la destinazione alla pila
          stack = stack.push(end)
        }
      }
      // colora di nero la zona attuale
      grayNodes = grayNodes.toSet.diff(Set(id)).toList
      blackNodes = blackNodes :+ id
    }
    // possiamo costruire a ritroso il percorso finale
    var id = end
    var finalRoute = List[step]()
    while(id != start) {
      val partialRoute = fatherRoutes(id)
      finalRoute = partialRoute ++ finalRoute
      id = getStepId(partialRoute.head)
    }
    return finalRoute
  }
  
  def createPedestrianPartialRouteFromTramStop(map : urban_elements, startEntity : String, direction : direction, target : String) : (String, List[step]) = {
    var steps = List[step]()
    var neighbor = ""
    // la startEntity può essere una tram stop o un pedestrian_crossroad
    if(getTramStop(map, startEntity).equals(None)) {
      // pedestrian_crossroad
      val pedestrian_crossroad = getPedestrianCrossroad(map, startEntity).get
      // crea lo step
      steps = steps :+ pedestrian_crossroad_step(pedestrian_crossroad, direction)
      // in base al verso di percorrenza si decide il vicino
      if(direction.beginToEnd == true) {
        // bisogna prendere il vicino su end
        neighbor = pedestrian_crossroad.coordinates.end.id
      }
      else {
        // bisogna prendere il vicino su begin
        neighbor = pedestrian_crossroad.coordinates.begin.id
      }
    }
    else {
      // tram stop
      val tramStop = getTramStop(map, startEntity).get
      // il lato di percorrenza della direzione è concorde con quello della zona
      assert(direction.position == tramStop.position)
      // crea lo step
      steps = steps :+ tram_stop_step(tramStop, direction, false)
      // in base al verso di percorrenza si decide il vicino
      if(direction.beginToEnd == true) {
        // bisogna prendere il vicino su end
        neighbor = tramStop.coordinates.end.id
      }
      else {
        // bisogna prendere il vicino su begin
        neighbor = tramStop.coordinates.begin.id
      }
    }
    var finished = false
    while(finished == false) {
      val (entity, x, y, z) = splitId(neighbor)
      // recupera l'id dello step precedente
      val previousId = getStepId(steps.last)
      // recupera la direction dello step precedente
      val previousDirection = getStepDirection(steps.last)
      entity match {
        case "road" =>
          // si crea lo step della strada con la stessa direzione precedente
          val road = getRoad(map, neighbor).get
          steps = steps :+ road_step(road, previousDirection)
          if(road.zonesIDs.contains(target) == true && previousDirection.position == getZone(map, target).get.position) {
            // è stata trovata la fine del percorso
            neighbor = target
          }
          // per decretare il vicino, basta guardare il beginToEnd della previousDirection
          else if(previousDirection.beginToEnd == true) {
            // restituisco il vicino end
            neighbor = road.coordinates.end.id
          }
          else {
            // restituisco il vicino begin
            neighbor = road.coordinates.begin.id
          }
        case "crossroad" =>
          // si applicano le regole per creare il nuovo step
          val crossroad = getCrossroad(map, neighbor).get
          var availableVertexes = crossroad.vertexes.filter { vertex => vertex.id != previousId && vertex.id != "nil"}
          // se la tipologia dell'incrocio è nil, allora ignoralo
          if(crossroad.category == category.nil) {
            // crea lo step in cui la direction è invariata
            steps = steps :+ crossroad_step(crossroad, previousDirection)
            // trova il vertice concorde alla direzione
            val previousVertex = getCorrespondingVertex(previousId, map, crossroad)
            availableVertexes = availableVertexes.toSet.diff(Set(previousVertex)).toList
            val (id, _) = getPedestrianCrossroadNeighbor(previousVertex, previousDirection, availableVertexes)
            neighbor = id
          }
          else {
            val previousVertex = getCorrespondingVertex(previousId, map, crossroad)
            availableVertexes = availableVertexes.toSet.diff(Set(previousVertex)).toList
            val (id, direction) = getPedestrianCrossroadNeighbor(previousVertex, previousDirection, availableVertexes)
            steps = steps :+ crossroad_step(crossroad, direction)
            neighbor = id
          }
        case "pedestrian_crossroad" =>
          // il percorso parziale termina qui
          // non include l'ultima entità
          finished = true
        case "bus_stop" =>
          // il bus stop viene ignorato momentaneamente
          val bus_stop = getBusStop(map, neighbor).get
          steps = steps :+ bus_stop_step(bus_stop, previousDirection, true)
          // per decretare il vicino, basta guardare il beginToEnd della previousDirection
          if(previousDirection.beginToEnd == true) {
            // restituisco il vicino end
            neighbor = bus_stop.coordinates.end.id
          }
          else {
            // restituisco il vicino begin
            neighbor = bus_stop.coordinates.begin.id
          }
        case "tram_stop" =>
          // il tram stop viene ignorato momentaneamente
          val tram_stop = getTramStop(map, neighbor).get
          steps = steps :+ tram_stop_step(tram_stop, previousDirection, true)
          // per decretare il vicino, basta guardare il beginToEnd della previousDirection
          if(previousDirection.beginToEnd == true) {
            // restituisco il vicino end
            neighbor = tram_stop.coordinates.end.id
          }
          else {
            // restituisco il vicino begin
            neighbor = tram_stop.coordinates.begin.id
          }
        case "zone" =>
          // il percorso parziale termina qui
          // non include l'ultima entità
          finished = true
      }
    }
    // neighbor contiene l'id dell'ultima entità, che è una pedestrian_crossroad o una zone
    return (neighbor, steps)
  }
  
  def createPedestrianPartialRouteToTramStop(map : urban_elements, startEntity : String, direction : direction, target : String) : (String, List[step]) = {
    var steps = List[step]()
    var neighbor = ""
    // la startEntity può essere una zone o un pedestrian_crossroad
    if(getZone(map, startEntity).equals(None)) {
      // pedestrian_crossroad
      val pedestrian_crossroad = getPedestrianCrossroad(map, startEntity).get
      // crea lo step
      steps = steps :+ pedestrian_crossroad_step(pedestrian_crossroad, direction)
      // in base al verso di percorrenza si decide il vicino
      if(direction.beginToEnd == true) {
        // bisogna prendere il vicino su end
        neighbor = pedestrian_crossroad.coordinates.end.id
      }
      else {
        // bisogna prendere il vicino su begin
        neighbor = pedestrian_crossroad.coordinates.begin.id
      }
    }
    else {
      // zone
      val zone = getZone(map, startEntity).get
      // il lato di percorrenza della direzione è concorde con quello della zona
      assert(direction.position == zone.position)
      // crea lo step
      steps = steps :+ zone_step(zone, direction)
      /*
      // in base al verso di percorrenza si decide il vicino
      if(direction.beginToEnd == true) {
        // bisogna prendere il vicino su end
        neighbor = zone.coordinates.end.id
      }
      else {
        // bisogna prendere il vicino su begin
        neighbor = zone.coordinates.begin.id
      }
      */
      // a prescindere dal verso di percorrenza, un zone step DEVE essere sempre seguito da un road step relativo alla road di appartenenza
      neighbor = zone.road
    }
    var finished = false
    while(finished == false) {
      val (entity, x, y, z) = splitId(neighbor)
      // recupera l'id dello step precedente
      val previousId = getStepId(steps.last)
      // recupera la direction dello step precedente
      val previousDirection = getStepDirection(steps.last)
      entity match {
        case "road" =>
          // si crea lo step della strada con la stessa direzione precedente
          val road = getRoad(map, neighbor).get
          steps = steps :+ road_step(road, previousDirection)
          // per decretare il vicino, basta guardare il beginToEnd della previousDirection
          if(previousDirection.beginToEnd == true) {
            // restituisco il vicino end
            neighbor = road.coordinates.end.id
          }
          else {
            // restituisco il vicino begin
            neighbor = road.coordinates.begin.id
          }
        case "crossroad" =>
          // si applicano le regole per creare il nuovo step
          val crossroad = getCrossroad(map, neighbor).get
          var availableVertexes = crossroad.vertexes.filter { vertex => vertex.id != previousId && vertex.id != "nil"}
          // se la tipologia dell'incrocio è nil, allora ignoralo
          if(crossroad.category == category.nil) {
            // crea lo step in cui la direction è invariata
            steps = steps :+ crossroad_step(crossroad, previousDirection)
            // trova il vertice concorde alla direzione
            val previousVertex = getCorrespondingVertex(previousId, map, crossroad)
            availableVertexes = availableVertexes.toSet.diff(Set(previousVertex)).toList
            val (id, _) = getPedestrianCrossroadNeighbor(previousVertex, previousDirection, availableVertexes)
            neighbor = id
          }
          else {
            val previousVertex = getCorrespondingVertex(previousId, map, crossroad)
            availableVertexes = availableVertexes.toSet.diff(Set(previousVertex)).toList
            val (id, direction) = getPedestrianCrossroadNeighbor(previousVertex, previousDirection, availableVertexes)
            steps = steps :+ crossroad_step(crossroad, direction)
            neighbor = id
          }
        case "pedestrian_crossroad" =>
          // il percorso parziale termina qui
          // non include l'ultima entità
          finished = true
        case "bus_stop" =>
          // il bus stop viene ignorato momentaneamente
          val bus_stop = getBusStop(map, neighbor).get
          steps = steps :+ bus_stop_step(bus_stop, previousDirection, true)
          // per decretare il vicino, basta guardare il beginToEnd della previousDirection
          if(previousDirection.beginToEnd == true) {
            // restituisco il vicino end
            neighbor = bus_stop.coordinates.end.id
          }
          else {
            // restituisco il vicino begin
            neighbor = bus_stop.coordinates.begin.id
          }
        case "tram_stop" =>
          val tram_stop = getTramStop(map, neighbor).get
          if(tram_stop.id == target && previousDirection.position == getTramStop(map, target).get.position) {
            // abbiamo finito
            // non includiamo l'ultima entità, come da norma
            finished = true
          }
          else {
            // il tram stop viene ignorato
            steps = steps :+ tram_stop_step(tram_stop, previousDirection, true)
            // per decretare il vicino, basta guardare il beginToEnd della previousDirection
            if(previousDirection.beginToEnd == true) {
              // restituisco il vicino end
              neighbor = tram_stop.coordinates.end.id
            }
            else {
              // restituisco il vicino begin
              neighbor = tram_stop.coordinates.begin.id
            }
          }
        case "zone" =>
          // non finiremo mai qui
          println("We should not be here!")
      }
    }
    // neighbor contiene l'id dell'ultima entità, che è un pedestrian crossroad o un tram stop
    return (neighbor, steps)
  }

}