package map

import Domain._
import Routes._
import Domain.position._

/**
 * @author Matteo Pozza
 * Metodi per la restituzione della sequenza di punti nell'attraversamento di un componente stradale
 */
object PointsSequence {
  
  val pedestrian_width = 1
  val pedestrian_length = 1
  
  val car_width = 4
  val car_length = 3
  
  val bus_width = 4
  val bus_length = 5
  
  val tram_width = 4
  val tram_length = 10
  
  // lista di liste tornata, perchè in alcuni casi potremmo avere dei pezzi dell'intera sequenza eseguiti in circostanze diverse
  def getPointsSequence(id : String, stepSequence : List[step]) : List[List[point]] = {
    // spacchetta la lista
    val previousPreviousStep = stepSequence(0)
    val previousStep = stepSequence(1)
    val currentStep = stepSequence(2)
    val nextStep = stepSequence(3)
    val nextNextStep = stepSequence(4)
    val nextNextNextStep = stepSequence(5)
    // comincia
    var finalList = List[List[point]]()
    // prima cosa, match sul tipo di entità mobile
    id.substring(0, 3) match {
      case "BUS" =>
        // seconda cosa: match sullo step
        currentStep match {
          case road_step(road, direction) =>
            // un bus non può avere road_step
            println("We should not be here!")
          case laneStep @ lane_step(lane, direction) =>
            // la direzione corrente è sicuramente la stessa di uscita
            finalList = handleLaneStep(laneStep, bus_length, previousStep, nextStep)
          case crossroadStep @ crossroad_step(crossroad, direction) =>
            finalList = handleCrossroadStep(crossroadStep, previousPreviousStep, previousStep, nextStep, nextNextStep, nextNextNextStep, bus_length)
          case pedestrianCrossroadStep @ pedestrian_crossroad_step(pedestrian_crossroad, direction) =>
            finalList = handlePedestrianCrossroadStep(pedestrianCrossroadStep, previousPreviousStep, previousStep, nextStep, bus_length)
          case busStopStep @ bus_stop_step(bus_stop, direction, ignore) =>
            finalList = handleBusStopBusStep(busStopStep, previousPreviousStep, previousStep, nextStep)
          case tramStopStep @ tram_stop_step(tram_stop, direction, ignore) =>
            finalList = handleTramStopStep(tramStopStep, previousPreviousStep, previousStep, nextStep, bus_length)
          case zone_step(zone, direction) =>
            // un bus non può avere zone_step
            println("We should not be here!")
        }
        
      case "TRA" =>
        // seconda cosa: match sullo step
        currentStep match {
          case road_step(road, direction) =>
            // un tram non può avere road_step
            println("We should not be here!")
          case laneStep @ lane_step(lane, direction) =>
            // la direzione corrente è sicuramente la stessa di uscita
            finalList = handleTramLaneStep(laneStep)
          case crossroadStep @ crossroad_step(crossroad, direction) =>
            finalList = handleCrossroadStep(crossroadStep, previousPreviousStep, previousStep, nextStep, nextNextStep, nextNextNextStep, tram_length)
          case pedestrianCrossroadStep @ pedestrian_crossroad_step(pedestrian_crossroad, direction) =>
            finalList = handlePedestrianCrossroadStep(pedestrianCrossroadStep, previousPreviousStep, previousStep, nextStep, tram_length)
          case busStopStep @ bus_stop_step(bus_stop, direction, ignore) =>
            finalList = handleBusStopStep(busStopStep, previousPreviousStep, previousStep, nextStep, tram_length)
          case tramStopStep @ tram_stop_step(tram_stop, direction, ignore) =>
            finalList = handleTramStopTramStep(tramStopStep, previousStep)
          case zone_step(zone, direction) =>
            // un tram non può avere zone_step
            println("We should not be here!")
        }
        
      case "PED" =>
        // seconda cosa: match sullo step
        currentStep match {
          case roadStep @ road_step(road, direction) =>
            finalList = handleRoadPedestrianStep(roadStep, previousStep, nextStep)
          case lane_step(lane, direction) =>
            // un pedone non può avere lane_step
            println("We should not be here!")
          case crossroadStep @ crossroad_step(crossroad, direction) =>
            finalList = handleCrossroadStep(crossroadStep, previousPreviousStep, previousStep, nextStep, nextNextStep, nextNextNextStep, pedestrian_length)
          case pedestrianCrossroadStep @ pedestrian_crossroad_step(pedestrian_crossroad, direction) =>
            finalList = handlePedestrianCrossroadPedestrianStep(pedestrianCrossroadStep, previousPreviousStep, previousStep)
          case busStopStep @ bus_stop_step(bus_stop, direction, ignore) =>
            finalList = handleBusStopPedestrianStep(busStopStep, previousPreviousStep, previousStep, nextStep, nextNextStep)
          case tramStopStep @ tram_stop_step(tram_stop, direction, ignore) =>
            finalList = handleTramStopPedestrianStep(tramStopStep, previousPreviousStep, previousStep, nextStep, nextNextStep)
          case zone_step(zone, direction) =>
            // la zone_step non ha bisogno di sequenze di punti
            println("We should not be here!")
        }
        
      case "CAR" =>
        // seconda cosa: match sullo step
        currentStep match {
          case road_step(road, direction) =>
            // una macchina non può avere road_step
            println("We should not be here!")
          case laneStep @ lane_step(lane, direction) =>
            // la direzione corrente è sicuramente la stessa di uscita
            finalList = handleLaneStep(laneStep, car_length, previousStep, nextStep)
          case crossroadStep @ crossroad_step(crossroad, direction) =>
            finalList = handleCrossroadStep(crossroadStep, previousPreviousStep, previousStep, nextStep, nextNextStep, nextNextNextStep, car_length)
          case pedestrianCrossroadStep @ pedestrian_crossroad_step(pedestrian_crossroad, direction) =>
            finalList = handlePedestrianCrossroadStep(pedestrianCrossroadStep, previousPreviousStep, previousStep, nextStep, car_length)
          case busStopStep @ bus_stop_step(bus_stop, direction, ignore) =>
            finalList = handleBusStopStep(busStopStep, previousPreviousStep, previousStep, nextStep, car_length)
          case tramStopStep @ tram_stop_step(tram_stop, direction, ignore) =>
            finalList = handleTramStopStep(tramStopStep, previousPreviousStep, previousStep, nextStep, car_length)
          case zone_step(zone, direction) =>
            // la zone_step non ha bisogno di sequenze di punti
            println("We should not be here!")
        }
 
    }
    
    return finalList
    
  }
  
  def handleTramLaneStep(laneStep : lane_step) : List[List[point]] = {
    var pointsList = List[List[point]]()
    laneStep.direction.position match {
      case `up` =>
        if(laneStep.direction.beginToEnd == true) {
          // caso non possibile, siamo contromarcia
          println("We should not be here!")
        }
        else {
          val laneEndPoint = JSONReader.getRoadEndPoint(current_map, laneStep.lane.road)
          val firstPointX = laneEndPoint._1 - tram_length
          val firstPointY = laneEndPoint._2 + 5
          var firstPointsList = List[point]()
          // recupera la x del punto di partenza
          val laneBeginPoint = JSONReader.getRoadBeginPoint(current_map, laneStep.lane.road)
          for(current_x <- firstPointX to laneBeginPoint._1 by -1) {
            val currentPoint = point(current_x, firstPointY)
            firstPointsList = firstPointsList :+ currentPoint
          }
          // aggiungi la lista
          pointsList = pointsList :+ firstPointsList
        }
      case `down` =>
        if(laneStep.direction.beginToEnd == true) {
          val laneBeginPoint = JSONReader.getRoadBeginPoint(current_map, laneStep.lane.road)
          val firstPointX = laneBeginPoint._1
          var firstPointY = laneBeginPoint._2 - 9
          var firstPointsList = List[point]()
          // recupera la x del punto di fine
          val laneEndPoint = JSONReader.getRoadEndPoint(current_map, laneStep.lane.road)
          for(current_x <- firstPointX to laneEndPoint._1 - tram_length) {
            val currentPoint = point(current_x, firstPointY)
            firstPointsList = firstPointsList :+ currentPoint
          }
          // aggiungi la lista
          pointsList = pointsList :+ firstPointsList
        }
        else {
          // caso non possibile, siamo contromarcia
          println("We should not be here!")
        }
      case `right` =>
        if(laneStep.direction.beginToEnd == true) {
          val laneBeginPoint = JSONReader.getRoadBeginPoint(current_map, laneStep.lane.road)
          val firstPointY= laneBeginPoint._2
          var firstPointX = laneBeginPoint._1 + 5
          var firstPointsList = List[point]()
          // recupera la y del punto di fine
          val laneEndPoint = JSONReader.getRoadEndPoint(current_map, laneStep.lane.road)
          for(current_y <- firstPointY to laneEndPoint._2 - tram_length) {
            val currentPoint = point(firstPointX, current_y)
            firstPointsList = firstPointsList :+ currentPoint
          }
          // aggiungi la lista
          pointsList = pointsList :+ firstPointsList
        }
        else {
          // caso non possibile, siamo contromarcia
          println("We should not be here!")
        }
      case `left` =>
        if(laneStep.direction.beginToEnd == true) {
          // caso non possibile, siamo contromarcia
          println("We should not be here!")
        }
        else {
          val laneEndPoint = JSONReader.getRoadEndPoint(current_map, laneStep.lane.road)
          val firstPointY= laneEndPoint._2 - tram_length
          var firstPointX = laneEndPoint._1 - 9
          var firstPointsList = List[point]()
          // recupera la y del punto di fine
          val laneBeginPoint = JSONReader.getRoadBeginPoint(current_map, laneStep.lane.road)
          for(current_y <- firstPointY to laneBeginPoint._2 by -1) {
            val currentPoint = point(firstPointX, current_y)
            firstPointsList = firstPointsList :+ currentPoint
          }
          // aggiungi la lista
          pointsList = pointsList :+ firstPointsList
        }
    }
    return pointsList
  }
  
  def handleLaneStep(laneStep : lane_step, vehicleLength : Int, previousStep : step, nextStep : step) : List[List[point]] = {
    var pointsList = List[List[point]]()
    // dal momento che un lane step non richiede particolari cambi di stato nel suo svolgimento,
    // solo la prima lista della lista di liste verrà utilizzata
    laneStep.direction.position match {
      case `up` =>
        if(laneStep.direction.beginToEnd == true) {
          // caso non possibile, siamo contromarcia
          println("We should not be here!")
        }
        else {
          // corsia orizzontale alta, da destra verso sinistra
          // bisogna capire se c'è una corsia del tram
          // se si, bisogna capire se è a fianco o meno
          val flags = tramLaneTest(laneStep.lane)
          val laneEndPoint = JSONReader.getRoadEndPoint(current_map, laneStep.lane.road)
          var firstPointX = 0
          previousStep match {
            case zone_step(zone, direction) =>
              // siamo usciti da un edificio
              firstPointX = zone.coordinates.begin.point.x + 6 - vehicleLength
            case _ =>
              firstPointX = laneEndPoint._1 - vehicleLength
          }
          var firstPointY = 0
          if(flags._1) {
            // tre corsie
            if(flags._2) {
              // corsia orizzontale più alta è del tram, corsia sottostante è la nostra
              firstPointY = laneEndPoint._2 - 2
            }
            else {
              // corsia orizzontale più bassa è quella del tram, la corsia più alta è la nostra
              firstPointY = laneEndPoint._2 + 5
            }
          }
          else {
            // due corsie
            firstPointY = laneEndPoint._2 + 2
          }
          var firstPointsList = List[point]()
          // recupera la x del punto di partenza
          val laneBeginPoint = JSONReader.getRoadBeginPoint(current_map, laneStep.lane.road)
          var limit = 0
          nextStep match {
            case zone_step(zone, direction) =>
              // stiamo entrando in un edificio
              limit = zone.coordinates.begin.point.x + 6
            case _ =>
              limit = laneBeginPoint._1
          }
          for(current_x <- firstPointX to limit by -1) {
            val currentPoint = point(current_x, firstPointY)
            firstPointsList = firstPointsList :+ currentPoint
          }
          // aggiungi la lista
          pointsList = pointsList :+ firstPointsList
        }
      case `down` =>
        if(laneStep.direction.beginToEnd == true) {
          // corsia orizzontale bassa, da sinistra verso destra
          // bisogna capire se c'è una corsia del tram
          // se si, bisogna capire se è a fianco o meno
          val flags = tramLaneTest(laneStep.lane)
          val laneBeginPoint = JSONReader.getRoadBeginPoint(current_map, laneStep.lane.road)
          var firstPointX = 0
          previousStep match {
            case zone_step(zone, direction) =>
              // siamo usciti da un edificio
              firstPointX = zone.coordinates.begin.point.x + 6
            case _ =>
              firstPointX = laneBeginPoint._1
          }
          var firstPointY = 0
          if(flags._1) {
            // tre corsie
            if(flags._2) {
              // corsia orizzontale più bassa è del tram, corsia sovrastante è la nostra
              firstPointY = laneBeginPoint._2 - 2
            }
            else {
              // corsia orizzontale più alta è quella del tram, la corsia più bassa è la nostra
              firstPointY = laneBeginPoint._2 - 9
            }
          }
          else {
            // due corsie
            firstPointY = laneBeginPoint._2 - 5
          }
          
          var firstPointsList = List[point]()
          // recupera la x del punto di fine
          val laneEndPoint = JSONReader.getRoadEndPoint(current_map, laneStep.lane.road)
          var limit = 0
          nextStep match {
            case zone_step(zone, direction) =>
              // stiamo entrando in un edificio
              limit = zone.coordinates.begin.point.x + 6 - vehicleLength
            case _ =>
              limit = laneEndPoint._1 - vehicleLength
          }
          for(current_x <- firstPointX to limit) {
            val currentPoint = point(current_x, firstPointY)
            firstPointsList = firstPointsList :+ currentPoint
          }
          // aggiungi la lista
          pointsList = pointsList :+ firstPointsList
        }
        else {
          // caso non possibile, siamo contromarcia
          println("We should not be here!")
        }
      case `left` =>
        if(laneStep.direction.beginToEnd == true) {
          // caso non possibile, siamo contromarcia
          println("We should not be here!")
        }
        else {
          // corsia verticale sinistra, dall'alto verso il basso
          // bisogna capire se c'è una corsia del tram
          // se si, bisogna capire se è a fianco o meno
          val flags = tramLaneTest(laneStep.lane)
          val laneEndPoint = JSONReader.getRoadEndPoint(current_map, laneStep.lane.road)
          var firstPointY = 0
          previousStep match {
            case zone_step(zone, direction) =>
              // siamo usciti da un edificio
              firstPointY = zone.coordinates.begin.point.y + 6 - vehicleLength
            case _ =>
              firstPointY = laneEndPoint._2 - vehicleLength
          }
          var firstPointX = 0
          if(flags._1) {
            // tre corsie
            if(flags._2) {
              // corsia verticale più sinistra è del tram, corsia a finaco è la nostra
              firstPointX = laneEndPoint._1 - 2
            }
            else {
              // corsia verticale più destra è quella del tram, la corsia più sinistra è la nostra
              firstPointX = laneEndPoint._1 - 9
            }
          }
          else {
            // due corsie
            firstPointX = laneEndPoint._1 - 5
          }
          
          var firstPointsList = List[point]()
          // recupera la y del punto di fine
          val laneBeginPoint = JSONReader.getRoadBeginPoint(current_map, laneStep.lane.road)
          var limit = 0
          nextStep match {
            case zone_step(zone, direction) =>
              // stiamo entrando in un edificio
              limit = zone.coordinates.begin.point.y + 6
            case _ =>
              limit = laneBeginPoint._2
          }
          for(current_y <- firstPointY to limit by -1) {
            val currentPoint = point(firstPointX, current_y)
            firstPointsList = firstPointsList :+ currentPoint
          }
          // aggiungi la lista
          pointsList = pointsList :+ firstPointsList
        }
      case `right` =>
        if(laneStep.direction.beginToEnd == true) {
          // corsia verticale destra, dal basso verso l'alto
          // bisogna capire se c'è una corsia del tram
          // se si, bisogna capire se è a fianco o meno
          val flags = tramLaneTest(laneStep.lane)
          val laneBeginPoint = JSONReader.getRoadBeginPoint(current_map, laneStep.lane.road)
          var firstPointY = 0
          previousStep match {
            case zone_step(zone, direction) =>
              // siamo usciti da un edificio
              firstPointY = zone.coordinates.begin.point.y + 6
            case _ =>
              firstPointY = laneBeginPoint._2
          }
          var firstPointX = 0
          if(flags._1) {
            // tre corsie
            if(flags._2) {
              // corsia verticale più destra è del tram, corsia a fianco è la nostra
              firstPointX = laneBeginPoint._1 - 2
            }
            else {
              // corsia verticale più sinistra è quella del tram, la corsia più destra è la nostra
              firstPointX = laneBeginPoint._1 + 5
            }
          }
          else {
            // due corsie
            firstPointX = laneBeginPoint._1 + 2
          }
          
          var firstPointsList = List[point]()
          // recupera la y del punto di fine
          val laneEndPoint = JSONReader.getRoadEndPoint(current_map, laneStep.lane.road)
          var limit = 0
          nextStep match {
            case zone_step(zone, direction) =>
              // stiamo entrando in un edificio
              limit = zone.coordinates.begin.point.y + 6 - vehicleLength
            case _ =>
              limit = laneEndPoint._2 - vehicleLength
          }
          for(current_y <- firstPointY to limit) {
            val currentPoint = point(firstPointX, current_y)
            firstPointsList = firstPointsList :+ currentPoint
          }
          // aggiungi la lista
          pointsList = pointsList :+ firstPointsList
        }
        else {
          // caso non possibile, siamo contromarcia
          println("We should not be here!")
        }
    }
    return pointsList
  }
  
  def tramLaneTest(lane : Domain.lane) : (Boolean, Boolean) = {
    // PRECONDIZIONE: la corsia in input non è una corsia di tram
    // il primo flag booleano dice se la corsia fa parte di una strada in cui è presente una corsia del tram
    // il secondo flag indica se la corsia del tram è affiancata a quella in input o meno
    
    // prima cosa: recupera la strada di appartenenza
    val road = JSONReader.getRoad(current_map, lane.road).get
    // esamina le corsie di appartenenza
    // tieni presente che una precondizione assunta è che una strada possa avere al massimo una sola corsia del tram
    for(id <- road.lanesIDs) {
      val currentLane = JSONReader.getLane(current_map, id).get
      if(currentLane.tram) {
        // abbiamo una corsia del tram: testiamo se è concorde a quella in input o meno
        if(currentLane.begintoend == lane.begintoend) {
          return (true, true)
        }
        else {
          return (true, false)
        }
      }
    }
    // non vi sono corsie del tram
    return (false, false)
  }
  
  def handleRoadPedestrianStep(roadStep : road_step, previousStep : step, nextStep : step) : List[List[point]] = {
	  var pointsList = List[List[point]]()
	  roadStep.direction.position match {
  	  case `up` =>
  	    if(roadStep.direction.beginToEnd) {
  		  val laneBeginPoint = JSONReader.getRoadBeginPoint(current_map, roadStep.road.id)
  			var firstPointX = 0
  			previousStep match {
  			  case zone_step(zone, direction) =>
  				  // siamo usciti da un edificio
  				  firstPointX = zone.coordinates.begin.point.x + 6
  				case _ =>
  				  firstPointX = laneBeginPoint._1
  		  }
  		  var firstPointY = 0
  		  if(roadStep.road.lanesIDs.length > 2) {
  			  // tre corsie
  			  firstPointY = laneBeginPoint._2 + 10
  		  }
  		  else {
  			  // due corsie
  			  firstPointY = laneBeginPoint._2 + 7
  		  }
  		  var firstPointsList = List[point]()
  		  // recupera la x del punto di fine
  		  val laneEndPoint = JSONReader.getRoadEndPoint(current_map, roadStep.road.id)
  		  var limit = 0
  		  nextStep match {
  				case zone_step(zone, direction) =>
  				  // stiamo entrando in un edificio
  				  limit = zone.coordinates.begin.point.x + 6 - pedestrian_length
  				case _ =>
  				  limit = laneEndPoint._1 - pedestrian_length
  		  }
  		  for(current_x <- firstPointX to limit) {
  			  val currentPoint = point(current_x, firstPointY)
  				firstPointsList = firstPointsList :+ currentPoint
  		  }
  		  // aggiungi la lista
  		  pointsList = pointsList :+ firstPointsList
  	  }
  	  else {
  		  val laneEndPoint = JSONReader.getRoadEndPoint(current_map, roadStep.road.id)
  		  var firstPointX = 0
  		  previousStep match {
  			  case zone_step(zone, direction) =>
  				  // siamo usciti da un edificio
  				  firstPointX = zone.coordinates.begin.point.x + 6 - pedestrian_length
  			  case _ =>
  				  firstPointX = laneEndPoint._1 - pedestrian_length
  		  }
  		  var firstPointY = 0
  		  if(roadStep.road.lanesIDs.length > 2) {
  			  // tre corsie
  			  firstPointY = laneEndPoint._2 + 11
  		  }
  		  else {
  			  // due corsie
  			  firstPointY = laneEndPoint._2 + 8
  		  }
  		  var firstPointsList = List[point]()
  		  // recupera la x del punto di partenza
  		  val laneBeginPoint = JSONReader.getRoadBeginPoint(current_map, roadStep.road.id)
  		  var limit = 0
  		  nextStep match {
  			  case zone_step(zone, direction) =>
  				  // stiamo entrando in un edificio
  				  limit = zone.coordinates.begin.point.x + 6
  			  case _ =>
  				  limit = laneBeginPoint._1
  		  }
  		  for(current_x <- firstPointX to limit by -1) {
  			  val currentPoint = point(current_x, firstPointY)
  			  firstPointsList = firstPointsList :+ currentPoint
  		  }
  		  // aggiungi la lista
  		  pointsList = pointsList :+ firstPointsList
  	  }
  	  case `down` =>
  	  if(roadStep.direction.beginToEnd) {
  		  val laneBeginPoint = JSONReader.getRoadBeginPoint(current_map, roadStep.road.id)
  		  var firstPointX = 0
  		  previousStep match {
  			  case zone_step(zone, direction) =>
  				  // siamo usciti da un edificio
  				  firstPointX = zone.coordinates.begin.point.x + 6
  			  case _ =>
  				  firstPointX = laneBeginPoint._1
  		  }
  		  var firstPointY = 0
  		  if(roadStep.road.lanesIDs.length > 2) {
  			  // tre corsie
  			  firstPointY = laneBeginPoint._2 - 12
  		  }
  		  else {
  			  // due corsie
  			  firstPointY = laneBeginPoint._2 - 8
  		  }
  		  var firstPointsList = List[point]()
  		  // recupera la x del punto di fine
  		  val laneEndPoint = JSONReader.getRoadEndPoint(current_map, roadStep.road.id)
  		  var limit = 0
  		  nextStep match {
  			  case zone_step(zone, direction) =>
  				  // stiamo entrando in un edificio
  				  limit = zone.coordinates.begin.point.x + 6 - pedestrian_length
  			  case _ =>
  				  limit = laneEndPoint._1 - pedestrian_length
  		  }
  		  for(current_x <- firstPointX to limit) {
  			  val currentPoint = point(current_x, firstPointY)
  			  firstPointsList = firstPointsList :+ currentPoint
  		  }
  		  // aggiungi la lista
  		  pointsList = pointsList :+ firstPointsList
  	  }
  	  else {
  		  val laneEndPoint = JSONReader.getRoadEndPoint(current_map, roadStep.road.id)
  		  var firstPointX = 0
  		  previousStep match {
  			  case zone_step(zone, direction) =>
  				  // siamo usciti da un edificio
  				  firstPointX = zone.coordinates.begin.point.x + 6 - pedestrian_length
  			  case _ =>
  				  firstPointX = laneEndPoint._1 - pedestrian_length
  		  }
  		  var firstPointY = 0
  		  if(roadStep.road.lanesIDs.length > 2) {
  			  // tre corsie
  			  firstPointY = laneEndPoint._2 - 11
  		  }
  		  else {
  			  // due corsie
  			  firstPointY = laneEndPoint._2 - 7
  		  }
  		  var firstPointsList = List[point]()
  		  // recupera la x del punto di partenza
  		  val laneBeginPoint = JSONReader.getRoadBeginPoint(current_map, roadStep.road.id)
  		  var limit = 0
  		  nextStep match {
  			  case zone_step(zone, direction) =>
  				  // stiamo entrando in un edificio
  				  limit = zone.coordinates.begin.point.x + 6
  			  case _ =>
  				  limit = laneBeginPoint._1
  		  }
  		  for(current_x <- firstPointX to limit by -1) {
  			  val currentPoint = point(current_x, firstPointY)
  			  firstPointsList = firstPointsList :+ currentPoint
  		  }
  		  // aggiungi la lista
  		  pointsList = pointsList :+ firstPointsList
  	  }
  	  case `left` =>
  	  if(roadStep.direction.beginToEnd) {
  		  val laneBeginPoint = JSONReader.getRoadBeginPoint(current_map, roadStep.road.id)
  		  var firstPointY = 0
  		  previousStep match {
  			  case zone_step(zone, direction) =>
  				  // siamo usciti da un edificio
  				  firstPointY = zone.coordinates.begin.point.y + 6
  			  case _ =>
  				  firstPointY = laneBeginPoint._2
  		  }
  		  var firstPointX = 0
  		  if(roadStep.road.lanesIDs.length > 2) {
  			  // tre corsie
  			  firstPointX = laneBeginPoint._1 - 11
  		  }
  		  else {
  			  // due corsie
  			  firstPointX = laneBeginPoint._1 - 7
  		  }
  		  var firstPointsList = List[point]()
  		  // recupera la y del punto di fine
  		  val laneEndPoint = JSONReader.getRoadEndPoint(current_map, roadStep.road.id)
  		  var limit = 0
  		  nextStep match {
  			  case zone_step(zone, direction) =>
  				  // stiamo entrando in un edificio
  				  limit = zone.coordinates.begin.point.y + 6 - pedestrian_length
  			  case _ =>
  				  limit = laneEndPoint._2 - pedestrian_length
  		  }
  		  for(current_y <- firstPointY to limit) {
  			  val currentPoint = point(firstPointX, current_y)
  			  firstPointsList = firstPointsList :+ currentPoint
  		  }
  		  // aggiungi la lista
  		  pointsList = pointsList :+ firstPointsList
  	  }
  	  else {
  		  val laneEndPoint = JSONReader.getRoadEndPoint(current_map, roadStep.road.id)
  		  var firstPointY = 0
  		  previousStep match {
  			  case zone_step(zone, direction) =>
  				  // siamo usciti da un edificio
  				  firstPointY = zone.coordinates.begin.point.y + 6 - pedestrian_length
  			  case _ =>
  				  firstPointY = laneEndPoint._2 - pedestrian_length
  		  }
  		  var firstPointX = 0
  		  if(roadStep.road.lanesIDs.length > 2) {
  			  // tre corsie
  			  firstPointX = laneEndPoint._1 - 12
  		  }
  		  else {
  			  // due corsie
  			  firstPointX = laneEndPoint._1 - 8
  		  }
  		  var firstPointsList = List[point]()
  		  // recupera la y del punto di fine
  		  val laneBeginPoint = JSONReader.getRoadBeginPoint(current_map, roadStep.road.id)
  		  var limit = 0
  		  nextStep match {
  			  case zone_step(zone, direction) =>
  				  // stiamo entrando in un edificio
  				  limit = zone.coordinates.begin.point.y + 6
  			  case _ =>
  				  limit = laneBeginPoint._2
  		  }
  		  for(current_y <- firstPointY to limit by -1) {
  			  val currentPoint = point(firstPointX, current_y)
  			  firstPointsList = firstPointsList :+ currentPoint
  		  }
  		  // aggiungi la lista
  		  pointsList = pointsList :+ firstPointsList
  	  }
  	  case `right` =>
  	  if(roadStep.direction.beginToEnd) {
  		  val laneBeginPoint = JSONReader.getRoadBeginPoint(current_map, roadStep.road.id)
  		  var firstPointY = 0
  		  previousStep match {
  			  case zone_step(zone, direction) =>
  				  // siamo usciti da un edificio
  				  firstPointY = zone.coordinates.begin.point.y + 6
  			  case _ =>
  				  firstPointY = laneBeginPoint._2
  		  }
  		  var firstPointX = 0
  		  if(roadStep.road.lanesIDs.length > 2) {
  			  // tre corsie
  			  firstPointX = laneBeginPoint._1 + 11
  		  }
  		  else {
  			  // due corsie
  			  firstPointX = laneBeginPoint._1 + 8
  		  }
  		  var firstPointsList = List[point]()
  		  // recupera la y del punto di fine
  		  val laneEndPoint = JSONReader.getRoadEndPoint(current_map, roadStep.road.id)
  		  var limit = 0
  		  nextStep match {
  			  case zone_step(zone, direction) =>
  				  // stiamo entrando in un edificio
  				  limit = zone.coordinates.begin.point.y + 6 - pedestrian_length
  			  case _ =>
  				  limit = laneEndPoint._2 - pedestrian_length
  		  }
  		  for(current_y <- firstPointY to limit) {
  			  val currentPoint = point(firstPointX, current_y)
  			  firstPointsList = firstPointsList :+ currentPoint
  		  }
  		  // aggiungi la lista
  		  pointsList = pointsList :+ firstPointsList
  	  }
  	  else {
  		  val laneEndPoint = JSONReader.getRoadEndPoint(current_map, roadStep.road.id)
  		  var firstPointY = 0
  		  previousStep match {
  			  case zone_step(zone, direction) =>
  				  // siamo usciti da un edificio
  				  firstPointY = zone.coordinates.begin.point.y + 6 - pedestrian_length
  			  case _ =>
  				  firstPointY = laneEndPoint._2 - pedestrian_length
  		  }
  		  var firstPointX = 0
  		  if(roadStep.road.lanesIDs.length > 2) {
  			  // tre corsie
  			  firstPointX = laneEndPoint._1 + 10
  		  }
  		  else {
  			  // due corsie
  			  firstPointX = laneEndPoint._1 + 7
  		  }
  		  var firstPointsList = List[point]()
  		  // recupera la y del punto di fine
  		  val laneBeginPoint = JSONReader.getRoadBeginPoint(current_map, roadStep.road.id)
  		  var limit = 0
  		  nextStep match {
  			  case zone_step(zone, direction) =>
  				  // stiamo entrando in un edificio
  				  limit = zone.coordinates.begin.point.y + 6
  			  case _ =>
  				  limit = laneBeginPoint._2
  		  }
  		  for(current_y <- firstPointY to limit by -1) {
  			  val currentPoint = point(firstPointX, current_y)
  			  firstPointsList = firstPointsList :+ currentPoint
  		  }
  		  // aggiungi la lista
  		  pointsList = pointsList :+ firstPointsList
  	  }
	  }
	  return pointsList
  }
  
  def handleBusStopPedestrianStep(busStopStep : bus_stop_step, previousPreviousStep : step, previousStep : step, nextStep : step, nextNextStep : step) : List[List[point]] = {
    var pointsList = List[List[point]]()
    if(busStopStep.ignore) {
      // sicuramente lo step precedente è un road_step
      previousStep match {
        case roadStep @ road_step(_, _) =>
          // recupera l'ultimo punto del percorso allo step precedente
          val previousSequence = handleRoadPedestrianStep(roadStep, previousPreviousStep, busStopStep)
          val lastPoint = previousSequence(0)(previousSequence(0).length - 1)
          var firstPointsList = List[point]()
          busStopStep.direction.position match {
            case `up` | `down` =>
              if(busStopStep.direction.beginToEnd) {
                for(current_x <- lastPoint.x + 1 to lastPoint.x + 24) {
                  val currentPoint = point(current_x, lastPoint.y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
              else {
                for(current_x <- lastPoint.x - 1 to lastPoint.x - 24 by -1) {
                  val currentPoint = point(current_x, lastPoint.y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
            case `left` | `right` =>
              if(busStopStep.direction.beginToEnd) {
                for(current_y <- lastPoint.y + 1 to lastPoint.y + 24) {
                  val currentPoint = point(lastPoint.x, current_y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
              else {
                for(current_y <- lastPoint.y - 1 to lastPoint.y - 24 by -1) {
                  val currentPoint = point(lastPoint.x, current_y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
          }
          pointsList = pointsList :+ firstPointsList
      }
    }
    else {
      // controlliamo se lo step precedente è un road step
      previousStep match {
        case roadStep @ road_step(_, _) =>
          // dobbiamo recuperare l'ultimo punto e agire di conseguenza
          val previousSequence = handleRoadPedestrianStep(roadStep, previousPreviousStep, busStopStep)
          val lastPoint = previousSequence(0)(previousSequence(0).length - 1)
          var firstPointsList = List[point]()
          busStopStep.direction.position match {
            case `up` | `down` =>
              if(busStopStep.direction.beginToEnd) {
                for(current_x <- lastPoint.x + 1 to lastPoint.x + 12) {
                  val currentPoint = point(current_x, lastPoint.y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
              else {
                for(current_x <- lastPoint.x - 1 to lastPoint.x - 12 by -1) {
                  val currentPoint = point(current_x, lastPoint.y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
            case `left` | `right` =>
              if(busStopStep.direction.beginToEnd) {
                for(current_y <- lastPoint.y + 1 to lastPoint.y + 12) {
                  val currentPoint = point(lastPoint.x, current_y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
              else {
                for(current_y <- lastPoint.y - 1 to lastPoint.y - 12 by -1) {
                  val currentPoint = point(lastPoint.x, current_y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
          }
          pointsList = pointsList :+ firstPointsList
        case _ =>
          // SICURAMENTE lo step successivo è un road_step
          // recuperiamo il primo punto e agiamo di conseguenza
          nextStep match {
            case roadStep @ road_step(_, _) =>
              val nextSequence = handleRoadPedestrianStep(roadStep, busStopStep, nextNextStep)
              val firstPoint = nextSequence(0)(0)
              var firstPointsList = List[point]()
              busStopStep.direction.position match {
                case `up` | `down` =>
                  if(busStopStep.direction.beginToEnd) {
                    for(current_x <- firstPoint.x - 12 to firstPoint.x - 1) {
                      val currentPoint = point(current_x, firstPoint.y)
                      firstPointsList = firstPointsList :+ currentPoint
                    }
                  }
                  else {
                    for(current_x <- firstPoint.x + 12 to firstPoint.x + 1 by -1) {
                      val currentPoint = point(current_x, firstPoint.y)
                      firstPointsList = firstPointsList :+ currentPoint
                    }
                  }
                case `left` | `right` =>
                  if(busStopStep.direction.beginToEnd) {
                    for(current_y <- firstPoint.y - 12 to firstPoint.y - 1) {
                      val currentPoint = point(firstPoint.x, current_y)
                      firstPointsList = firstPointsList :+ currentPoint
                    }
                  }
                  else {
                    for(current_y <- firstPoint.y +12 to firstPoint.y + 1 by -1) {
                      val currentPoint = point(firstPoint.x, current_y)
                      firstPointsList = firstPointsList :+ currentPoint
                    }
                  }
              }
              pointsList = pointsList :+ firstPointsList
            case _ =>
              println("We should not be here!")
          }
      }
    }
    return pointsList
  }
  
  def handleBusStopBusStep(busStopStep : bus_stop_step, previousPreviousStep : step, previousStep : step, nextStep : step) : List[List[point]] = {
    var pointsList = List[List[point]]()
    if(busStopStep.ignore) {
      // restituisco un unico percorso
      
      // sicuramente lo step precedente è un lane_step
      previousStep match {
        case laneStep @ lane_step(_, _) =>
          val previousSequence = handleLaneStep(laneStep, bus_length, previousPreviousStep, nextStep)
          val lastPoint = previousSequence(0)(previousSequence(0).length - 1)
          var firstPointsList = List[point]()
          busStopStep.direction.position match {
            case `up` | `down` =>
              if(busStopStep.direction.beginToEnd) {
                for(current_x <- lastPoint.x + 1 to lastPoint.x + 24 + bus_length - 1) {
                  val currentPoint = point(current_x, lastPoint.y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
              else {
                for(current_x <- lastPoint.x - 1 to lastPoint.x - 24 - bus_length + 1 by -1) {
                  val currentPoint = point(current_x, lastPoint.y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
            case `left` | `right` =>
              if(busStopStep.direction.beginToEnd) {
                for(current_y <- lastPoint.y + 1 to lastPoint.y + 24 + bus_length - 1) {
                  val currentPoint = point(lastPoint.x, current_y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
              else {
                for(current_y <- lastPoint.y - 1 to lastPoint.y - 24 - bus_length + 1 by -1) {
                  val currentPoint = point(lastPoint.x, current_y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
          }
          pointsList = pointsList :+ firstPointsList
        case _ =>
          println("We should not be here!")
      }
    }
    else {
      // restituisco due pezzi di percorso (fino alla metà e dalla metà in poi)
      
      // sicuramente lo step precedente è un lane_step
      previousStep match {
        case laneStep @ lane_step(_, _) =>
          val previousSequence = handleLaneStep(laneStep, bus_length, previousPreviousStep, nextStep)
          val lastPoint = previousSequence(0)(previousSequence(0).length - 1)
          var firstPointsList = List[point]()
          var secondPointsList = List[point]()
          busStopStep.direction.position match {
            case `up` | `down` =>
              if(busStopStep.direction.beginToEnd) {
                for(current_x <- lastPoint.x + 1 to lastPoint.x + ((24 + bus_length - 1) / 2)) {
                  val currentPoint = point(current_x, lastPoint.y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
                for(current_x <- lastPoint.x + ((24 + bus_length - 1) / 2) + 1 to lastPoint.x + 24 + bus_length - 1) {
                  val currentPoint = point(current_x, lastPoint.y)
                  secondPointsList = secondPointsList :+ currentPoint
                }
              }
              else {
                for(current_x <- lastPoint.x - 1 to lastPoint.x - ((24 + bus_length - 1) / 2) by -1) {
                  val currentPoint = point(current_x, lastPoint.y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
                for(current_x <- lastPoint.x - ((24 + bus_length - 1) / 2) - 1 to lastPoint.x - 24 - bus_length + 1 by -1) {
                  val currentPoint = point(current_x, lastPoint.y)
                  secondPointsList = secondPointsList :+ currentPoint
                }
              }
            case `left` | `right` =>
              if(busStopStep.direction.beginToEnd) {for(current_y <- lastPoint.y + 1 to lastPoint.y + ((24 + bus_length - 1) / 2)) {
                  val currentPoint = point(lastPoint.x, current_y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
                for(current_y <- lastPoint.y + ((24 + bus_length - 1) / 2) + 1 to lastPoint.y + 24 + bus_length - 1) {
                  val currentPoint = point(lastPoint.x, current_y)
                  secondPointsList = secondPointsList :+ currentPoint
                }
              }
              else {
                for(current_y <- lastPoint.y - 1 to lastPoint.y - ((24 + bus_length - 1) / 2) by -1) {
                  val currentPoint = point(lastPoint.x, current_y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
                for(current_y <- lastPoint.y - ((24 + bus_length - 1) / 2) - 1 to lastPoint.y - 24 - bus_length + 1 by -1) {
                  val currentPoint = point(lastPoint.x, current_y)
                  secondPointsList = secondPointsList :+ currentPoint
                }
              }
          }
          pointsList = pointsList :+ firstPointsList
          pointsList = pointsList :+ secondPointsList
        case _ =>
          println("We should not be here!")
      }
    }
    return pointsList
  }
  
  def handleBusStopStep(busStopStep : bus_stop_step, previousPreviousStep : step, previousStep : step, nextStep : step, vehicleLength : Int) : List[List[point]] = {
    var pointsList = List[List[point]]()
    // SICURAMENTE ignore == true
    if(busStopStep.ignore) {
      // restituisco un unico percorso
      
      // sicuramente lo step precedente è un lane_step
      previousStep match {
        case laneStep @ lane_step(_, _) =>
          var previousSequence = List[List[point]]()
          if(vehicleLength == car_length) {
            previousSequence = handleLaneStep(laneStep, vehicleLength, previousPreviousStep, nextStep)
          }
          else if(vehicleLength == tram_length) {
            previousSequence = handleTramLaneStep(laneStep)
          }
          val lastPoint = previousSequence(0)(previousSequence(0).length - 1)
          var firstPointsList = List[point]()
          busStopStep.direction.position match {
            case `up` | `down` =>
              if(busStopStep.direction.beginToEnd) {
                for(current_x <- lastPoint.x + 1 to lastPoint.x + 24 + vehicleLength - 1) {
                  val currentPoint = point(current_x, lastPoint.y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
              else {
                for(current_x <- lastPoint.x - 1 to lastPoint.x - 24 - vehicleLength + 1 by -1) {
                  val currentPoint = point(current_x, lastPoint.y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
            case `left` | `right` =>
              if(busStopStep.direction.beginToEnd) {
                for(current_y <- lastPoint.y + 1 to lastPoint.y + 24 + vehicleLength - 1) {
                  val currentPoint = point(lastPoint.x, current_y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
              else {
                for(current_y <- lastPoint.y - 1 to lastPoint.y - 24 - vehicleLength + 1 by -1) {
                  val currentPoint = point(lastPoint.x, current_y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
          }
          pointsList = pointsList :+ firstPointsList
        case _ =>
          println("We should not be here!")
      }
    }
    else {
      println("We should not be here!")
    }
    return pointsList
  }
  
  def handleTramStopPedestrianStep(tramStopStep : tram_stop_step, previousPreviousStep : step, previousStep : step, nextStep : step, nextNextStep : step) : List[List[point]] = {
    var pointsList = List[List[point]]()
    if(tramStopStep.ignore) {
      // sicuramente lo step precedente è un road_step
      previousStep match {
        case roadStep @ road_step(_, _) =>
          // recupera l'ultimo punto del percorso allo step precedente
          val previousSequence = handleRoadPedestrianStep(roadStep, previousPreviousStep, tramStopStep)
          val lastPoint = previousSequence(0)(previousSequence(0).length - 1)
          var firstPointsList = List[point]()
          tramStopStep.direction.position match {
            case `up` | `down` =>
              if(tramStopStep.direction.beginToEnd) {
                for(current_x <- lastPoint.x + 1 to lastPoint.x + 24) {
                  val currentPoint = point(current_x, lastPoint.y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
              else {
                for(current_x <- lastPoint.x - 1 to lastPoint.x - 24 by -1) {
                  val currentPoint = point(current_x, lastPoint.y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
            case `left` | `right` =>
              if(tramStopStep.direction.beginToEnd) {
                for(current_y <- lastPoint.y + 1 to lastPoint.y + 24) {
                  val currentPoint = point(lastPoint.x, current_y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
              else {
                for(current_y <- lastPoint.y - 1 to lastPoint.y - 24 by -1) {
                  val currentPoint = point(lastPoint.x, current_y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
          }
          pointsList = pointsList :+ firstPointsList
      }
    }
    else {
      // controlliamo se lo step precedente è un road step
      previousStep match {
        case roadStep @ road_step(_, _) =>
          // dobbiamo recuperare l'ultimo punto e agire di conseguenza
          val previousSequence = handleRoadPedestrianStep(roadStep, previousPreviousStep, tramStopStep)
          val lastPoint = previousSequence(0)(previousSequence(0).length - 1)
          var firstPointsList = List[point]()
          tramStopStep.direction.position match {
            case `up` | `down` =>
              if(tramStopStep.direction.beginToEnd) {
                for(current_x <- lastPoint.x + 1 to lastPoint.x + 12) {
                  val currentPoint = point(current_x, lastPoint.y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
              else {
                for(current_x <- lastPoint.x - 1 to lastPoint.x - 12 by -1) {
                  val currentPoint = point(current_x, lastPoint.y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
            case `left` | `right` =>
              if(tramStopStep.direction.beginToEnd) {
                for(current_y <- lastPoint.y + 1 to lastPoint.y + 12) {
                  val currentPoint = point(lastPoint.x, current_y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
              else {
                for(current_y <- lastPoint.y - 1 to lastPoint.y - 12 by -1) {
                  val currentPoint = point(lastPoint.x, current_y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
          }
          pointsList = pointsList :+ firstPointsList
        case _ =>
          // SICURAMENTE lo step successivo è un road_step
          // recuperiamo il primo punto e agiamo di conseguenza
          nextStep match {
            case roadStep @ road_step(_, _) =>
              val nextSequence = handleRoadPedestrianStep(roadStep, tramStopStep, nextNextStep)
              val firstPoint = nextSequence(0)(0)
              var firstPointsList = List[point]()
              tramStopStep.direction.position match {
                case `up` | `down` =>
                  if(tramStopStep.direction.beginToEnd) {
                    for(current_x <- firstPoint.x - 12 to firstPoint.x - 1) {
                      val currentPoint = point(current_x, firstPoint.y)
                      firstPointsList = firstPointsList :+ currentPoint
                    }
                  }
                  else {
                    for(current_x <- firstPoint.x + 12 to firstPoint.x + 1 by -1) {
                      val currentPoint = point(current_x, firstPoint.y)
                      firstPointsList = firstPointsList :+ currentPoint
                    }
                  }
                case `left` | `right` =>
                  if(tramStopStep.direction.beginToEnd) {
                    for(current_y <- firstPoint.y - 12 to firstPoint.y - 1) {
                      val currentPoint = point(firstPoint.x, current_y)
                      firstPointsList = firstPointsList :+ currentPoint
                    }
                  }
                  else {
                    for(current_y <- firstPoint.y +12 to firstPoint.y + 1 by -1) {
                      val currentPoint = point(firstPoint.x, current_y)
                      firstPointsList = firstPointsList :+ currentPoint
                    }
                  }
              }
              pointsList = pointsList :+ firstPointsList
            case _ =>
              println("We should not be here!")
          }
      }
    }
    return pointsList
  }
  
  def handleTramStopTramStep(tramStopStep : tram_stop_step, previousStep : step) : List[List[point]] = {
    var pointsList = List[List[point]]()
    if(tramStopStep.ignore) {
      // restituisco un unico percorso
      
      // sicuramente lo step precedente è un lane_step
      previousStep match {
        case laneStep @ lane_step(_, _) =>
          val previousSequence = handleTramLaneStep(laneStep)
          val lastPoint = previousSequence(0)(previousSequence(0).length - 1)
          var firstPointsList = List[point]()
          tramStopStep.direction.position match {
            case `up` | `down` =>
              if(tramStopStep.direction.beginToEnd) {
                for(current_x <- lastPoint.x + 1 to lastPoint.x + 24 + tram_length - 1) {
                  val currentPoint = point(current_x, lastPoint.y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
              else {
                for(current_x <- lastPoint.x - 1 to lastPoint.x - 24 - tram_length + 1 by -1) {
                  val currentPoint = point(current_x, lastPoint.y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
            case `left` | `right` =>
              if(tramStopStep.direction.beginToEnd) {
                for(current_y <- lastPoint.y + 1 to lastPoint.y + 24 + tram_length - 1) {
                  val currentPoint = point(lastPoint.x, current_y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
              else {
                for(current_y <- lastPoint.y - 1 to lastPoint.y - 24 - tram_length + 1 by -1) {
                  val currentPoint = point(lastPoint.x, current_y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
          }
          pointsList = pointsList :+ firstPointsList
        case _ =>
          println("We should not be here!")
      }
    }
    else {
      // restituisco due pezzi di percorso (fino alla metà e dalla metà in poi)
      
      // sicuramente lo step precedente è un lane_step
      previousStep match {
        case laneStep @ lane_step(_, _) =>
          val previousSequence = handleTramLaneStep(laneStep)
          val lastPoint = previousSequence(0)(previousSequence(0).length - 1)
          var firstPointsList = List[point]()
          var secondPointsList = List[point]()
          tramStopStep.direction.position match {
            case `up` | `down` =>
              if(tramStopStep.direction.beginToEnd) {
                for(current_x <- lastPoint.x + 1 to lastPoint.x + ((24 + tram_length - 1) / 2)) {
                  val currentPoint = point(current_x, lastPoint.y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
                for(current_x <- lastPoint.x + ((24 + tram_length - 1) / 2) + 1 to lastPoint.x + 24 + tram_length - 1) {
                  val currentPoint = point(current_x, lastPoint.y)
                  secondPointsList = secondPointsList :+ currentPoint
                }
              }
              else {
                for(current_x <- lastPoint.x - 1 to lastPoint.x - ((24 + tram_length - 1) / 2) by -1) {
                  val currentPoint = point(current_x, lastPoint.y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
                for(current_x <- lastPoint.x - ((24 + tram_length - 1) / 2) - 1 to lastPoint.x - 24 - tram_length + 1 by -1) {
                  val currentPoint = point(current_x, lastPoint.y)
                  secondPointsList = secondPointsList :+ currentPoint
                }
              }
            case `left` | `right` =>
              if(tramStopStep.direction.beginToEnd) {
                for(current_y <- lastPoint.y + 1 to lastPoint.y + ((24 + tram_length - 1) / 2)) {
                  val currentPoint = point(lastPoint.x, current_y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
                for(current_y <- lastPoint.y + ((24 + tram_length - 1) / 2) + 1 to lastPoint.y + 24 + tram_length - 1) {
                  val currentPoint = point(lastPoint.x, current_y)
                  secondPointsList = secondPointsList :+ currentPoint
                }
              }
              else {
                for(current_y <- lastPoint.y - 1 to lastPoint.y - ((24 + tram_length - 1) / 2) by -1) {
                  val currentPoint = point(lastPoint.x, current_y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
                for(current_y <- lastPoint.y - ((24 + tram_length - 1) / 2) - 1 to lastPoint.y - 24 - tram_length + 1 by -1) {
                  val currentPoint = point(lastPoint.x, current_y)
                  secondPointsList = secondPointsList :+ currentPoint
                }
              }
          }
          pointsList = pointsList :+ firstPointsList
          pointsList = pointsList :+ secondPointsList
        case _ =>
          println("We should not be here!")
      }
    }
    return pointsList
  }
  
  def handleTramStopStep(tramStopStep : tram_stop_step, previousPreviousStep : step, previousStep : step, nextStep : step, vehicleLength : Int) : List[List[point]] = {
    var pointsList = List[List[point]]()
    // SICURAMENTE ignore == true
    if(tramStopStep.ignore) {
      // restituisco un unico percorso
      
      // sicuramente lo step precedente è un lane_step
      previousStep match {
        case laneStep @ lane_step(_, _) =>
          var previousSequence = List[List[point]]()
          previousSequence = handleLaneStep(laneStep, vehicleLength, previousPreviousStep, nextStep)
          val lastPoint = previousSequence(0)(previousSequence(0).length - 1)
          var firstPointsList = List[point]()
          tramStopStep.direction.position match {
            case `up` | `down` =>
              if(tramStopStep.direction.beginToEnd) {
                for(current_x <- lastPoint.x + 1 to lastPoint.x + 24 + vehicleLength - 1) {
                  val currentPoint = point(current_x, lastPoint.y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
              else {
                for(current_x <- lastPoint.x - 1 to lastPoint.x - 24 - vehicleLength + 1 by -1) {
                  val currentPoint = point(current_x, lastPoint.y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
            case `left` | `right` =>
              if(tramStopStep.direction.beginToEnd) {
                for(current_y <- lastPoint.y + 1 to lastPoint.y + 24 + vehicleLength - 1) {
                  val currentPoint = point(lastPoint.x, current_y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
              else {
                for(current_y <- lastPoint.y - 1 to lastPoint.y - 24 - vehicleLength + 1 by -1) {
                  val currentPoint = point(lastPoint.x, current_y)
                  firstPointsList = firstPointsList :+ currentPoint
                }
              }
          }
          pointsList = pointsList :+ firstPointsList
        case _ =>
          println("We should not be here!")
      }
    }
    else {
      println("We should not be here!")
    }
    return pointsList
  }
  
  def handlePedestrianCrossroadStep(pedestrianCrossroadStep : pedestrian_crossroad_step, previousPreviousStep : step, previousStep : step, nextStep : step, vehicleLength : Int) : List[List[point]] = {
    var pointsList = List[List[point]]()
    // sicuramente lo step precedente è un lane_step
    previousStep match {
      case laneStep @ lane_step(_, _) =>
        var previousSequence = List[List[point]]()
        if(vehicleLength == car_length || vehicleLength == bus_length) {
          previousSequence = handleLaneStep(laneStep, vehicleLength, previousPreviousStep, nextStep)
        }
        else if(vehicleLength == tram_length) {
          previousSequence = handleTramLaneStep(laneStep)
        }
        val lastPoint = previousSequence(0)(previousSequence(0).length - 1)
        var firstPointsList = List[point]()
        pedestrianCrossroadStep.direction.position match {
          case `up` | `down` =>
            if(pedestrianCrossroadStep.direction.beginToEnd) {
              for(current_x <- lastPoint.x + 1 to lastPoint.x + 12 + vehicleLength - 1) {
                val currentPoint = point(current_x, lastPoint.y)
                firstPointsList = firstPointsList :+ currentPoint
              }
            }
            else {
              for(current_x <- lastPoint.x - 1 to lastPoint.x - 12 - vehicleLength + 1 by -1) {
                val currentPoint = point(current_x, lastPoint.y)
                firstPointsList = firstPointsList :+ currentPoint
              }
            }
          case `left` | `right` =>
            if(pedestrianCrossroadStep.direction.beginToEnd) {
              for(current_y <- lastPoint.y + 1 to lastPoint.y + 12 + vehicleLength - 1) {
                val currentPoint = point(lastPoint.x, current_y)
                firstPointsList = firstPointsList :+ currentPoint
              }
            }
            else {
              for(current_y <- lastPoint.y - 1 to lastPoint.y - 12 - vehicleLength + 1 by -1) {
                val currentPoint = point(lastPoint.x, current_y)
                firstPointsList = firstPointsList :+ currentPoint
              }
            }
        }
        pointsList = pointsList :+ firstPointsList
      case _ =>
        println("We should not be here!")
    }
    return pointsList
  }
  
  def calcShift(start : point, offset : Int, arrow : position) : List[point] = {
    // PRECONDIZIONE: primo punto escluso, ultimo punto incluso
    var pointList = List[point]()
    arrow match {
      case `up` =>
        for(current_y <- start.y + 1 to start.y + offset) {
          pointList = pointList :+ point(start.x, current_y)
        }
      case `down` =>
        for(current_y <- start.y - 1 to start.y - offset by -1) {
          pointList = pointList :+ point(start.x, current_y)
        }
      case `left` =>
        for(current_x <- start.x - 1 to start.x - offset by -1) {
          pointList = pointList :+ point(current_x, start.y)
        }
      case `right` =>
        for(current_x <- start.x + 1 to start.x + offset) {
          pointList = pointList :+ point(current_x, start.y)
        }
    }
    return pointList
  }
  
  def handlePedestrianCrossroadPedestrianStep(pedestrianCrossroadStep : pedestrian_crossroad_step, previousPreviousStep : step, previousStep : step) : List[List[point]] = {
    var pointsList = List[List[point]]()
    previousStep match {
      case roadStep @ road_step(road, direction) =>
        // recupera l'ultimo punto del percorso allo step precedente
        val previousSequence = handleRoadPedestrianStep(roadStep, previousPreviousStep, pedestrianCrossroadStep)
        val lastPoint = previousSequence(0)(previousSequence(0).length - 1)
        direction.position match {
          case `up` =>
            if(direction.beginToEnd) {
              pedestrianCrossroadStep.direction.position match {
                case `up` =>
                  // stiamo ignorando le strisce
                  val firstPointsList = calcShift(lastPoint, 12, `right`)
                  pointsList = pointsList :+ firstPointsList
                case `down` =>
                  // prima parte
                  val firstPointsList = calcShift(lastPoint, 6, `right`)
                  // recuperiamo il numero di corsie della strada
                  var secondPointsList = List[point]()
                  var thirdPointsList = List[point]()
                  if(road.lanesIDs.length < 3) {
                    // due corsie
                    secondPointsList = calcShift(firstPointsList(firstPointsList.length - 1), 14, `down`)
                  }
                  else {
                    // tre corsie
                    secondPointsList = calcShift(firstPointsList(firstPointsList.length - 1), 21, `down`)
                  }
                  if(pedestrianCrossroadStep.direction.beginToEnd) {
                    secondPointsList = secondPointsList :+ calcShift(secondPointsList(secondPointsList.length - 1), 1, `down`)(0)
                    thirdPointsList = calcShift(secondPointsList(secondPointsList.length - 1), 6, `right`)
                  }
                  else {
                    thirdPointsList = calcShift(secondPointsList(secondPointsList.length - 1), 6, `left`)
                  }
                  pointsList = pointsList :+ firstPointsList
                  pointsList = pointsList :+ secondPointsList
                  pointsList = pointsList :+ thirdPointsList
                case _ =>
                  println("We should not be here!")
              }
            }
            else {
              pedestrianCrossroadStep.direction.position match {
                case `up` =>
                  // stiamo ignorando le strisce
                  val firstPointsList = calcShift(lastPoint, 12, `left`)
                  pointsList = pointsList :+ firstPointsList
                case `down` =>
                  // prima parte
                  val firstPointsList = calcShift(lastPoint, 6, `left`)
                  // recuperiamo il numero di corsie della strada
                  var secondPointsList = List[point]()
                  var thirdPointsList = List[point]()
                  if(road.lanesIDs.length < 3) {
                    // due corsie
                    secondPointsList = calcShift(firstPointsList(firstPointsList.length - 1), 15, `down`)
                  }
                  else {
                    // tre corsie
                    secondPointsList = calcShift(firstPointsList(firstPointsList.length - 1), 22, `down`)
                  }
                  if(pedestrianCrossroadStep.direction.beginToEnd) {
                    secondPointsList = secondPointsList :+ calcShift(secondPointsList(secondPointsList.length - 1), 1, `down`)(0)
                    thirdPointsList = calcShift(secondPointsList(secondPointsList.length - 1), 6, `right`)
                  }
                  else {
                    thirdPointsList = calcShift(secondPointsList(secondPointsList.length - 1), 6, `left`)
                  }
                  pointsList = pointsList :+ firstPointsList
                  pointsList = pointsList :+ secondPointsList
                  pointsList = pointsList :+ thirdPointsList
                case _ =>
                  println("We should not be here!")
              }
            }
          case `down` =>
            if(direction.beginToEnd) {
              pedestrianCrossroadStep.direction.position match {
                case `down` =>
                  // stiamo ignorando le strisce
                  val firstPointsList = calcShift(lastPoint, 12, `right`)
                  pointsList = pointsList :+ firstPointsList
                case `up` =>
                  // prima parte
                  val firstPointsList = calcShift(lastPoint, 6, `right`)
                  // recuperiamo il numero di corsie della strada
                  var secondPointsList = List[point]()
                  var thirdPointsList = List[point]()
                  if(road.lanesIDs.length < 3) {
                    // due corsie
                    secondPointsList = calcShift(firstPointsList(firstPointsList.length - 1), 15, `up`)
                  }
                  else {
                    // tre corsie
                    secondPointsList = calcShift(firstPointsList(firstPointsList.length - 1), 22, `up`)
                  }
                  if(pedestrianCrossroadStep.direction.beginToEnd) {
                    thirdPointsList = calcShift(secondPointsList(secondPointsList.length - 1), 6, `right`)
                  }
                  else {
                    secondPointsList = secondPointsList :+ calcShift(secondPointsList(secondPointsList.length - 1), 1, `up`)(0)
                    thirdPointsList = calcShift(secondPointsList(secondPointsList.length - 1), 6, `left`)
                  }
                  pointsList = pointsList :+ firstPointsList
                  pointsList = pointsList :+ secondPointsList
                  pointsList = pointsList :+ thirdPointsList
                case _ =>
                  println("We should not be here!")
              }
            }
            else {
              pedestrianCrossroadStep.direction.position match {
                case `down` =>
                  // stiamo ignorando le strisce
                  val firstPointsList = calcShift(lastPoint, 12, `left`)
                  pointsList = pointsList :+ firstPointsList
                case `up` =>
                  // prima parte
                  val firstPointsList = calcShift(lastPoint, 6, `left`)
                  // recuperiamo il numero di corsie della strada
                  var secondPointsList = List[point]()
                  var thirdPointsList = List[point]()
                  if(road.lanesIDs.length < 3) {
                    // due corsie
                    secondPointsList = calcShift(firstPointsList(firstPointsList.length - 1), 14, `up`)
                  }
                  else {
                    // tre corsie
                    secondPointsList = calcShift(firstPointsList(firstPointsList.length - 1), 21, `up`)
                  }
                  if(pedestrianCrossroadStep.direction.beginToEnd) {
                    thirdPointsList = calcShift(secondPointsList(secondPointsList.length - 1), 6, `right`)
                  }
                  else {
                    secondPointsList = secondPointsList :+ calcShift(secondPointsList(secondPointsList.length - 1), 1, `up`)(0)
                    thirdPointsList = calcShift(secondPointsList(secondPointsList.length - 1), 6, `left`)
                  }
                  pointsList = pointsList :+ firstPointsList
                  pointsList = pointsList :+ secondPointsList
                  pointsList = pointsList :+ thirdPointsList
                case _ =>
                  println("We should not be here!")
              }
            }
          case `left` =>
            if(direction.beginToEnd) {
              pedestrianCrossroadStep.direction.position match {
                case `left` =>
                  // stiamo ignorando le strisce
                  val firstPointsList = calcShift(lastPoint, 12, `up`)
                  pointsList = pointsList :+ firstPointsList
                case `right` =>
                  // prima parte
                  val firstPointsList = calcShift(lastPoint, 6, `up`)
                  // recuperiamo il numero di corsie della strada
                  var secondPointsList = List[point]()
                  var thirdPointsList = List[point]()
                  if(road.lanesIDs.length < 3) {
                    // due corsie
                    secondPointsList = calcShift(firstPointsList(firstPointsList.length - 1), 14, `right`)
                  }
                  else {
                    // tre corsie
                    secondPointsList = calcShift(firstPointsList(firstPointsList.length - 1), 21, `right`)
                  }
                  if(pedestrianCrossroadStep.direction.beginToEnd) {
                    secondPointsList = secondPointsList :+ calcShift(secondPointsList(secondPointsList.length - 1), 1, `right`)(0)
                    thirdPointsList = calcShift(secondPointsList(secondPointsList.length - 1), 6, `up`)
                  }
                  else {
                    thirdPointsList = calcShift(secondPointsList(secondPointsList.length - 1), 6, `down`)
                  }
                  pointsList = pointsList :+ firstPointsList
                  pointsList = pointsList :+ secondPointsList
                  pointsList = pointsList :+ thirdPointsList
                case _ =>
                  println("We should not be here!")
              }
            }
            else {
              pedestrianCrossroadStep.direction.position match {
                case `left` =>
                  // stiamo ignorando le strisce
                  val firstPointsList = calcShift(lastPoint, 12, `down`)
                  pointsList = pointsList :+ firstPointsList
                case `right` =>
                  // prima parte
                  val firstPointsList = calcShift(lastPoint, 6, `down`)
                  // recuperiamo il numero di corsie della strada
                  var secondPointsList = List[point]()
                  var thirdPointsList = List[point]()
                  if(road.lanesIDs.length < 3) {
                    // due corsie
                    secondPointsList = calcShift(firstPointsList(firstPointsList.length - 1), 15, `right`)
                  }
                  else {
                    // tre corsie
                    secondPointsList = calcShift(firstPointsList(firstPointsList.length - 1), 22, `right`)
                  }
                  if(pedestrianCrossroadStep.direction.beginToEnd) {
                    secondPointsList = secondPointsList :+ calcShift(secondPointsList(secondPointsList.length - 1), 1, `right`)(0)
                    thirdPointsList = calcShift(secondPointsList(secondPointsList.length - 1), 6, `up`)
                  }
                  else {
                    thirdPointsList = calcShift(secondPointsList(secondPointsList.length - 1), 6, `down`)
                  }
                  pointsList = pointsList :+ firstPointsList
                  pointsList = pointsList :+ secondPointsList
                  pointsList = pointsList :+ thirdPointsList
                case _ =>
                  println("We should not be here!")
              }
            }
          case `right` =>
            if(direction.beginToEnd) {
              pedestrianCrossroadStep.direction.position match {
                case `right` =>
                  // stiamo ignorando le strisce
                  val firstPointsList = calcShift(lastPoint, 12, `up`)
                  pointsList = pointsList :+ firstPointsList
                case `left` =>
                  // prima parte
                  val firstPointsList = calcShift(lastPoint, 6, `up`)
                  // recuperiamo il numero di corsie della strada
                  var secondPointsList = List[point]()
                  var thirdPointsList = List[point]()
                  if(road.lanesIDs.length < 3) {
                    // due corsie
                    secondPointsList = calcShift(firstPointsList(firstPointsList.length - 1), 15, `left`)
                  }
                  else {
                    // tre corsie
                    secondPointsList = calcShift(firstPointsList(firstPointsList.length - 1), 22, `left`)
                  }
                  if(pedestrianCrossroadStep.direction.beginToEnd) {
                    thirdPointsList = calcShift(secondPointsList(secondPointsList.length - 1), 6, `up`)
                  }
                  else {
                    secondPointsList = secondPointsList :+ calcShift(secondPointsList(secondPointsList.length - 1), 1, `left`)(0)
                    thirdPointsList = calcShift(secondPointsList(secondPointsList.length - 1), 6, `down`)
                  }
                  pointsList = pointsList :+ firstPointsList
                  pointsList = pointsList :+ secondPointsList
                  pointsList = pointsList :+ thirdPointsList
                case _ =>
                  println("We should not be here!")
              }
            }
            else {
              pedestrianCrossroadStep.direction.position match {
                case `right` =>
                  // stiamo ignorando le strisce
                  val firstPointsList = calcShift(lastPoint, 12, `down`)
                  pointsList = pointsList :+ firstPointsList
                case `left` =>
                  // prima parte
                  val firstPointsList = calcShift(lastPoint, 6, `down`)
                  // recuperiamo il numero di corsie della strada
                  var secondPointsList = List[point]()
                  var thirdPointsList = List[point]()
                  if(road.lanesIDs.length < 3) {
                    // due corsie
                    secondPointsList = calcShift(firstPointsList(firstPointsList.length - 1), 14, `left`)
                  }
                  else {
                    // tre corsie
                    secondPointsList = calcShift(firstPointsList(firstPointsList.length - 1), 21, `left`)
                  }
                  if(pedestrianCrossroadStep.direction.beginToEnd) {
                    thirdPointsList = calcShift(secondPointsList(secondPointsList.length - 1), 6, `up`)
                  }
                  else {
                    secondPointsList = secondPointsList :+ calcShift(secondPointsList(secondPointsList.length - 1), 1, `left`)(0)
                    thirdPointsList = calcShift(secondPointsList(secondPointsList.length - 1), 6, `down`)
                  }
                  pointsList = pointsList :+ firstPointsList
                  pointsList = pointsList :+ secondPointsList
                  pointsList = pointsList :+ thirdPointsList
                case _ =>
                  println("We should not be here!")
              }
            }
        }
      case _ =>
        println("We should not be here!")
    }
    return pointsList
  }
  
  def handleCrossroadStep(crossroadStep : crossroad_step, previousPreviousStep : step, previousStep : step, nextStep : step, nextNextStep : step, nextNextNextStep : step, entityLength : Int) : List[List[point]] = {
    var currStep =  crossroadStep;
    var pointsList = List[List[point]]()
    // vi sono solo due possibilità: o lo step precedente è un road_step (pedone)
    // o lo step precedente è un lane_step (veicolo)
    var beginPoint = point(0, 0)
    var endPoint = point(0, 0)
    var previousDirection = direction(`up`, true) // valory dummy
    previousStep match {
      case previousRoadStep @ road_step(previousRoad, direction) =>
        // sono un pedone
        previousDirection = direction
        val previousSequence = handleRoadPedestrianStep(previousRoadStep, previousPreviousStep, crossroadStep)
        beginPoint = previousSequence(0)(previousSequence(0).length - 1)
        nextStep match {
          // DEVE essere un road_step o un crossroad_step
          case nextRoadStep @ road_step(nextRoad, _) =>
            val nextSequence = handleRoadPedestrianStep(nextRoadStep, crossroadStep, nextNextStep)
            endPoint = nextSequence(0)(0)
          case nextCrossroadStep @ crossroad_step(nextCrossroad, _) =>
            currStep = nextCrossroadStep;
            // dobbiamo recuperare il punto dallo step ancora successivo, che DEVE essere un road step
            nextNextStep match {
              case nextNextRoadStep @ road_step(nextNextRoad, _) =>
                val nextNextSequence = handleRoadPedestrianStep(nextNextRoadStep, nextStep, nextNextNextStep)
                endPoint = nextNextSequence(0)(0)
              case _ =>
                println("We should not be here!")
            }
          case _ =>
            println("We should not be here!")
        }
      case previousLaneStep @ lane_step(previousLane, direction) =>
        // sono un veicolo
        // bisogna sempre distinguere se sono un tram o meno, perchè la gestione del lane step non è la stessa
        previousDirection = direction
        if(entityLength == tram_length) {
          val previousSequence = handleTramLaneStep(previousLaneStep)
          beginPoint = previousSequence(0)(previousSequence(0).length - 1)
        }
        else {
          val previousSequence = handleLaneStep(previousLaneStep, entityLength, previousPreviousStep, crossroadStep)
          beginPoint = previousSequence(0)(previousSequence(0).length - 1)
        }
        nextStep match {
          // DEVE essere un lane_step o un crossroad_step
          case nextLaneStep @ lane_step(nextLane, _) =>
            if(entityLength == tram_length) {
              val nextSequence = handleTramLaneStep(nextLaneStep)
              endPoint = nextSequence(0)(0)
            }
            else {
              val nextSequence = handleLaneStep(nextLaneStep, entityLength, crossroadStep, nextNextStep)
              endPoint = nextSequence(0)(0)
            }
          case nextCrossroadStep @ crossroad_step(nextCrossroad, _) =>
            currStep = nextCrossroadStep;
          // dobbiamo recuperare il punto dallo step ancora successivo, che DEVE essere un lane step
          nextNextStep match {
            case nextNextLaneStep @ lane_step(nextNextLane, _) =>
              if(entityLength == tram_length) {
                val nextNextSequence = handleTramLaneStep(nextNextLaneStep)
                endPoint = nextNextSequence(0)(0)
              }
              else {
                val nextNextSequence = handleLaneStep(nextNextLaneStep, entityLength, nextStep, nextNextNextStep)
                endPoint = nextNextSequence(0)(0)
              }
            case _ =>
              println("We should not be here!")
          }
        case _ =>
          println("We should not be here!")
        }
      case _ =>
        println("We should not be here!")
    }
    // beginPoint ed endPoint sono definiti
    // previousDirection è definita
    // beginLanes e endLanes sono definiti
    previousDirection.position match {
      case `up` =>
        if(previousDirection.beginToEnd) {
          // solo un pedone può raggiungere un incrocio da up e beginToEnd == true
          currStep.direction.position match {
            case `up` =>
              if(currStep.direction.beginToEnd) {
                // controlla la y per decidere se il numero di corsie è diverso
                val offset = endPoint.x - beginPoint.x
                if(beginPoint.y == endPoint.y) {
                  // stesso numero di corsie
                  val firstPointsList = calcShift(beginPoint, offset - 1, `right`)
                  pointsList = pointsList :+ firstPointsList
                }
                else {
                  // numero di corsie differente
                  // lo facciamo avanzare fino alla sua lunghezza, poi lo muoviamo per il resto del percorso
                  val firstPointsList = calcShift(beginPoint, entityLength, `right`)
                  val newPoint = point(beginPoint.x + entityLength, endPoint.y)
                  val secondPointsList = calcShift(newPoint, offset - entityLength - 1, `right`)
                  pointsList = pointsList :+ (firstPointsList ++ secondPointsList)
                }
              }
              else {
                // nessuna entità può entrare ed uscire da un incrocio con due direzioni opposte
                println("We should not be here!")
              }
            case `down` =>
              if(currStep.direction.beginToEnd) {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
              else {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
            case `left` =>
              if(currStep.direction.beginToEnd) {
                // prima parte
                val offsetX = endPoint.x - beginPoint.x
                val offsetY = endPoint.y - beginPoint.y
                val firstPointsList = calcShift(beginPoint, offsetX - entityLength, `right`)
                // seconda parte
                val newPoint = point(endPoint.x, beginPoint.y - 1)
                val secondPointsList = calcShift(newPoint, offsetY, `up`)
                pointsList = pointsList :+ firstPointsList
                pointsList = pointsList :+ secondPointsList
              }
              else {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
            case `right` =>
              if(currStep.direction.beginToEnd) {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
              else {
                // prima parte
                val offsetX = endPoint.x - beginPoint.x
                val offsetY = beginPoint.y - endPoint.y
                val firstPointsList = calcShift(beginPoint, offsetX - entityLength, `right`)
                // seconda parte
                val newPoint = point(endPoint.x, beginPoint.y + 1)
                val secondPointsList = calcShift(newPoint, offsetY, `down`)
                pointsList = pointsList :+ firstPointsList
                pointsList = pointsList :+ secondPointsList
              }
          }
        }
        else {
          // qui possiamo avere sia veicoli che pedoni
          currStep.direction.position match {
            case `up` =>
              if(currStep.direction.beginToEnd) {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
              else {
                // controlla la y per decidere se il numero di corsie è diverso
                val offset = beginPoint.x - endPoint.x
                if(beginPoint.y == endPoint.y) {
                  // stesso numero di corsie
                  val firstPointsList = calcShift(beginPoint, offset - 1, `left`)
                  pointsList = pointsList :+ firstPointsList
                }
                else {
                  // numero di corsie differente
                  // lo facciamo avanzare fino alla sua lunghezza, poi lo muoviamo per il resto del percorso
                  val firstPointsList = calcShift(beginPoint, entityLength, `left`)
                  val newPoint = point(beginPoint.x - entityLength, endPoint.y)
                  val secondPointsList = calcShift(newPoint, offset - entityLength - 1, `left`)
                  pointsList = pointsList :+ (firstPointsList ++ secondPointsList)
                }
              }
            case `down` =>
              if(currStep.direction.beginToEnd) {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
              else {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
            case `left` =>
              if(currStep.direction.beginToEnd) {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
              else {
                // prima parte
                val offsetX = beginPoint.x - endPoint.x
                val offsetY = beginPoint.y - endPoint.y - entityLength
                val firstPointsList = calcShift(beginPoint, offsetX, `left`)
                // seconda parte
                val newPoint = point(endPoint.x, beginPoint.y + entityLength - 1)
                val secondPointsList = calcShift(newPoint, offsetY, `down`)
                pointsList = pointsList :+ firstPointsList
                pointsList = pointsList :+ secondPointsList
              }
            case `right` =>
              if(currStep.direction.beginToEnd) {
                // prima parte
                val offsetX = beginPoint.x - endPoint.x
                val offsetY = endPoint.y - beginPoint.y
                val firstPointsList = calcShift(beginPoint, offsetX, `left`)
                // seconda parte
                val newPoint = point(endPoint.x, beginPoint.y)
                val secondPointsList = calcShift(newPoint, offsetY - 1, `up`)
                pointsList = pointsList :+ firstPointsList
                pointsList = pointsList :+ secondPointsList
              }
              else {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
          }
        }
      case `down` =>
        if(previousDirection.beginToEnd) {
          currStep.direction.position match {
            case `up` =>
              if(currStep.direction.beginToEnd) {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
              else {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
            case `down` =>
              if(currStep.direction.beginToEnd) {
                // controlla la y per decidere se il numero di corsie è diverso
                val offset = endPoint.x - beginPoint.x
                if(beginPoint.y == endPoint.y) {
                  // stesso numero di corsie
                  val firstPointsList = calcShift(beginPoint, offset - 1, `right`)
                  pointsList = pointsList :+ firstPointsList
                }
                else {
                  // numero di corsie differente
                  // lo facciamo avanzare fino alla sua lunghezza, poi lo muoviamo per il resto del percorso
                  val firstPointsList = calcShift(beginPoint, entityLength, `right`)
                  val newPoint = point(beginPoint.x + entityLength, endPoint.y)
                  val secondPointsList = calcShift(newPoint, offset - entityLength - 1, `right`)
                  pointsList = pointsList :+ (firstPointsList ++ secondPointsList)
                }
              }
              else {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
            case `left` =>
              if(currStep.direction.beginToEnd) {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
              else {
                // prima parte
                val offsetX = endPoint.x - beginPoint.x - entityLength
                val offsetY = beginPoint.y - endPoint.y - entityLength
                val firstPointsList = calcShift(beginPoint, offsetX, `right`)
                // seconda parte
                val newPoint = point(endPoint.x, beginPoint.y - entityLength + 1)
                val secondPointsList = calcShift(newPoint, offsetY, `down`)
                pointsList = pointsList :+ firstPointsList
                pointsList = pointsList :+ secondPointsList
              }
            case `right` =>
              if(currStep.direction.beginToEnd) {
                // prima parte
                val offsetX = endPoint.x - beginPoint.x - entityLength
                val offsetY = endPoint.y - beginPoint.y
                val firstPointsList = calcShift(beginPoint, offsetX, `right`)
                // seconda parte
                val newPoint = point(endPoint.x, beginPoint.y - 1)
                val secondPointsList = calcShift(newPoint, offsetY, `up`)
                pointsList = pointsList :+ firstPointsList
                pointsList = pointsList :+ secondPointsList
              }
              else {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
          }
        }
        else {
          currStep.direction.position match {
            case `up` =>
              if(currStep.direction.beginToEnd) {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
              else {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
            case `down` =>
              if(currStep.direction.beginToEnd) {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
              else {
                // controlla la y per decidere se il numero di corsie è diverso
                val offset = beginPoint.x - endPoint.x
                if(beginPoint.y == endPoint.y) {
                  // stesso numero di corsie
                  val firstPointsList = calcShift(beginPoint, offset - 1, `left`)
                  pointsList = pointsList :+ firstPointsList
                }
                else {
                  // numero di corsie differente
                  // lo facciamo avanzare fino alla sua lunghezza, poi lo muoviamo per il resto del percorso
                  val firstPointsList = calcShift(beginPoint, entityLength, `left`)
                  val newPoint = point(beginPoint.x - entityLength, endPoint.y)
                  val secondPointsList = calcShift(newPoint, offset - entityLength - 1, `left`)
                  pointsList = pointsList :+ (firstPointsList ++ secondPointsList)
                }
              }
            case `left` =>
              if(currStep.direction.beginToEnd) {
                // prima parte
                val offsetX = beginPoint.x - endPoint.x
                val offsetY = endPoint.y - beginPoint.y
                val firstPointsList = calcShift(beginPoint, offsetX, `left`)
                // seconda parte
                val newPoint = point(endPoint.x, beginPoint.y)
                val secondPointsList = calcShift(newPoint, offsetY - 1, `up`)
                pointsList = pointsList :+ firstPointsList
                pointsList = pointsList :+ secondPointsList
              }
              else {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
            case `right` =>
              if(currStep.direction.beginToEnd) {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
              else {
                // prima parte
                val offsetX = beginPoint.x - endPoint.x
                val offsetY = beginPoint.y - endPoint.y - entityLength
                val firstPointsList = calcShift(beginPoint, offsetX, `left`)
                // seconda parte
                val newPoint = point(endPoint.x, beginPoint.y - entityLength + 1)
                val secondPointsList = calcShift(newPoint, offsetY, `down`)
                pointsList = pointsList :+ firstPointsList
                pointsList = pointsList :+ secondPointsList
              }
          }
        }
      case `left` =>
        if(previousDirection.beginToEnd) {
          // siamo sicuramente un pedone
          currStep.direction.position match {
            case `up` =>
              if(currStep.direction.beginToEnd) {
                // prima parte
                val offsetX = endPoint.x - beginPoint.x
                val offsetY = endPoint.y - beginPoint.y - entityLength
                val firstPointsList = calcShift(beginPoint, offsetY, `up`)
                // seconda parte
                val newPoint = point(beginPoint.x - 1, endPoint.y)
                val secondPointsList = calcShift(newPoint, offsetX, `right`)
                pointsList = pointsList :+ firstPointsList
                pointsList = pointsList :+ secondPointsList
              }
              else {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
            case `down` =>
              if(currStep.direction.beginToEnd) {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
              else {
                // prima parte
                val offsetX = beginPoint.x - endPoint.x - entityLength
                val offsetY = endPoint.y - beginPoint.y - entityLength
                val firstPointsList = calcShift(beginPoint, offsetY, `up`)
                // seconda parte
                val newPoint = point(beginPoint.x - entityLength + 1, endPoint.y)
                val secondPointsList = calcShift(newPoint, offsetX, `left`)
                pointsList = pointsList :+ firstPointsList
                pointsList = pointsList :+ secondPointsList
              }
            case `left` =>
              if(currStep.direction.beginToEnd) {
                // controlla la x per decidere se il numero di corsie è diverso
                val offset = endPoint.y - beginPoint.y
                if(beginPoint.x == endPoint.x) {
                  // stesso numero di corsie
                  val firstPointsList = calcShift(beginPoint, offset - 1, `up`)
                  pointsList = pointsList :+ firstPointsList
                }
                else {
                  // numero di corsie differente
                  // lo facciamo avanzare fino alla sua lunghezza, poi lo muoviamo per il resto del percorso
                  val firstPointsList = calcShift(beginPoint, entityLength, `up`)
                  val newPoint = point(endPoint.x, beginPoint.y + entityLength)
                  val secondPointsList = calcShift(newPoint, offset - entityLength - 1, `up`)
                  pointsList = pointsList :+ (firstPointsList ++ secondPointsList)
                }
              }
              else {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
            case `right` =>
              if(currStep.direction.beginToEnd) {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
              else {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
          }
        }
        else {
          // possiamo essere sia veicolo sia pedone
          currStep.direction.position match {
            case `up` =>
              if(currStep.direction.beginToEnd) {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
              else {
                // prima parte
                val offsetX = beginPoint.x - endPoint.x - entityLength
                val offsetY = beginPoint.y - endPoint.y
                val firstPointsList = calcShift(beginPoint, offsetY, `down`)
                // seconda parte
                val newPoint = point(beginPoint.x - entityLength + 1, endPoint.y)
                val secondPointsList = calcShift(newPoint, offsetX, `left`)
                pointsList = pointsList :+ firstPointsList
                pointsList = pointsList :+ secondPointsList
              }
            case `down` =>
              if(currStep.direction.beginToEnd) {
                // prima parte
                val offsetX = endPoint.x - beginPoint.x
                val offsetY = beginPoint.y - endPoint.y
                val firstPointsList = calcShift(beginPoint, offsetY, `down`)
                // seconda parte
                val newPoint = point(beginPoint.x, endPoint.y)
                val secondPointsList = calcShift(newPoint, offsetX - 1, `right`)
                pointsList = pointsList :+ firstPointsList
                pointsList = pointsList :+ secondPointsList
              }
              else {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
            case `left` =>
              if(currStep.direction.beginToEnd) {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
              else {
                // controlla la x per decidere se il numero di corsie è diverso
                val offset = beginPoint.y - endPoint.y
                if(beginPoint.x == endPoint.x) {
                  // stesso numero di corsie
                  val firstPointsList = calcShift(beginPoint, offset - 1, `down`)
                  pointsList = pointsList :+ firstPointsList
                }
                else {
                  // numero di corsie differente
                  // lo facciamo avanzare fino alla sua lunghezza, poi lo muoviamo per il resto del percorso
                  val firstPointsList = calcShift(beginPoint, entityLength, `down`)
                  val newPoint = point(endPoint.x, beginPoint.y - entityLength)
                  val secondPointsList = calcShift(newPoint, offset - entityLength - 1, `down`)
                  pointsList = pointsList :+ (firstPointsList ++ secondPointsList)
                }
              }
            case `right` =>
              if(currStep.direction.beginToEnd) {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
              else {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
          }
        }
      case `right` =>
        if(previousDirection.beginToEnd) {
          // possiamo essere sia veicolo sia pedone
          currStep.direction.position match {
            case `up` =>
              if(currStep.direction.beginToEnd) {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
              else {
                // prima parte
                val offsetX = beginPoint.x - endPoint.x - entityLength
                val offsetY = endPoint.y - beginPoint.y - entityLength
                val firstPointsList = calcShift(beginPoint, offsetY, `up`)
                // seconda parte
                val newPoint = point(beginPoint.x - entityLength + 1, endPoint.y)
                val secondPointsList = calcShift(newPoint, offsetX, `left`)
                pointsList = pointsList :+ firstPointsList
                pointsList = pointsList :+ secondPointsList
              }
            case `down` =>
              if(currStep.direction.beginToEnd) {
                // prima parte
                val offsetX = endPoint.x - beginPoint.x
                val offsetY = endPoint.y - beginPoint.y - entityLength
                val firstPointsList = calcShift(beginPoint, offsetY, `up`)
                // seconda parte
                val newPoint = point(beginPoint.x - 1, endPoint.y)
                val secondPointsList = calcShift(newPoint, offsetX, `right`)
                pointsList = pointsList :+ firstPointsList
                pointsList = pointsList :+ secondPointsList
              }
              else {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
            case `left` =>
              if(currStep.direction.beginToEnd) {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
              else {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
            case `right` =>
              if(currStep.direction.beginToEnd) {
                // controlla la x per decidere se il numero di corsie è diverso
                val offset = endPoint.y - beginPoint.y
                if(beginPoint.x == endPoint.x) {
                  // stesso numero di corsie
                  val firstPointsList = calcShift(beginPoint, offset - 1, `up`)
                  pointsList = pointsList :+ firstPointsList
                }
                else {
                  // numero di corsie differente
                  // lo facciamo avanzare fino alla sua lunghezza, poi lo muoviamo per il resto del percorso
                  val firstPointsList = calcShift(beginPoint, entityLength, `up`)
                  val newPoint = point(endPoint.x, beginPoint.y + entityLength)
                  val secondPointsList = calcShift(newPoint, offset - entityLength - 1, `up`)
                  pointsList = pointsList :+ (firstPointsList ++ secondPointsList)
                }
              }
              else {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
          }
        }
        else {
          // siamo sicuramente un pedone
          currStep.direction.position match {
            case `up` =>
              if(currStep.direction.beginToEnd) {
                // prima parte
                val offsetX = endPoint.x - beginPoint.x
                val offsetY = beginPoint.y - endPoint.y
                val firstPointsList = calcShift(beginPoint, offsetY, `down`)
                // seconda parte
                val newPoint = point(beginPoint.x, endPoint.y)
                val secondPointsList = calcShift(newPoint, offsetX - 1, `right`)
                pointsList = pointsList :+ firstPointsList
                pointsList = pointsList :+ secondPointsList
              }
              else {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
            case `down` =>
              if(currStep.direction.beginToEnd) {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
              else {
                // prima parte
                val offsetX = beginPoint.x - endPoint.x - entityLength
                val offsetY = beginPoint.y - endPoint.y
                val firstPointsList = calcShift(beginPoint, offsetY, `down`)
                // seconda parte
                val newPoint = point(beginPoint.x - entityLength + 1, endPoint.y)
                val secondPointsList = calcShift(newPoint, offsetX, `left`)
                pointsList = pointsList :+ firstPointsList
                pointsList = pointsList :+ secondPointsList
              }
            case `left` =>
              if(currStep.direction.beginToEnd) {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
              else {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
            case `right` =>
              if(currStep.direction.beginToEnd) {
                // non è possibile avere questa combinazione
                println("We should not be here!")
              }
              else {
                // controlla la x per decidere se il numero di corsie è diverso
                val offset = beginPoint.y - endPoint.y
                if(beginPoint.x == endPoint.x) {
                  // stesso numero di corsie
                  val firstPointsList = calcShift(beginPoint, offset - 1, `down`)
                  pointsList = pointsList :+ firstPointsList
                }
                else {
                  // numero di corsie differente
                  // lo facciamo avanzare fino alla sua lunghezza, poi lo muoviamo per il resto del percorso
                  val firstPointsList = calcShift(beginPoint, entityLength, `down`)
                  val newPoint = point(endPoint.x, beginPoint.y - entityLength)
                  val secondPointsList = calcShift(newPoint, offset - entityLength - 1, `down`)
                  pointsList = pointsList :+ (firstPointsList ++ secondPointsList)
                }
              }
          }
        }
    }
    return pointsList
  }
  
  // UTILITY
  // dato un id, restituisce la lunghezza dell'entità
  def getLengthFromId(id : String) : Int = {
    id.substring(0, 3) match {
      case "PED" =>
        return pedestrian_length
      case "CAR" =>
        return car_length
      case "BUS" =>
        return bus_length
      case "TRA" =>
        return tram_length
    }
  }
  
}