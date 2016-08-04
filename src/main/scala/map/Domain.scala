package map;
import org.json4s._
import org.json4s.JsonDSL._
import JSONUtilities._
import JSONReader._

// classi di dominio

object Domain {

	object category extends Enumeration {
		type category = Value
				val nil = Value("nil")
				val angle = Value("angle")
				val classic = Value("classic")
				val semaphore = Value("semaphore")
				val roundabout = Value("roundabout")

		def printCategory(category : category) : String = {
			category match {
			case `nil` => return "nil"
			case `angle` => return "angle"
			case `classic` => return "classic"
			case `semaphore` => return "semaphore"
			case `roundabout` => return "roundabout"
			}
		}
    
	}

	object position extends Enumeration {
		type position = Value
				val up = Value("up")
				val down = Value("down")
				val left = Value("left")
				val right = Value("right")

		def printPosition(position : position) : String = {
			position match {
			case `up` => return "up"
			case `down` => return "down"
			case `left` => return "left"
			case `right` => return "right"
			}
		}
    
    def getOpposite(position : position) : position = {
      position match {
        case `up` => return down
        case `down` => return up
        case `left` => return right
        case `right` => return left
      }
    }

	}

	object variety extends Enumeration {
		type variety = Value
				val houseplace = Value("houseplace")
				val workplace = Value("workplace")
				val funplace = Value("funplace")

		def printVariety(variety : variety) : String = {
			variety match {
			case `houseplace` => return "houseplace"
			case `workplace` => return "workplace"
			case `funplace` => return "funplace"
			}
		}

	}

	import category._
	import position._
	import variety._
  case class dimensions(x : Int, y : Int)
  case class point(x : Int, y : Int)
  case class begin(point : point, id : String)
  case class end(point : point, id : String)
  case class coordinates(begin : begin, end : end)
  case class road(id : String, coordinates : coordinates, lanesIDs : List[String], zonesIDs : List[String])
  case class lane(id : String, road : String, begintoend : Boolean, tram : Boolean, bus_routes : List[Int])
  case class vertex(point : point, id : String)
  case class crossroad(id : String, vertexes : List[vertex], category : category, extendable : Boolean)
  case class pedestrian_crossroad(id : String, coordinates : coordinates)
  case class bus_stop(id : String, coordinates : coordinates, position : position, route : Int)
  case class tram_stop(id : String, coordinates : coordinates, position : position, route : Int)
  case class zone(id : String, road : String, coordinates : coordinates, variety : variety, position : position)
  case class urban_elements(dimensions : dimensions,
                            roads : List[road],
		                        lanes : List[lane],
		                        crossroads : List[crossroad],
		                        pedestrian_crossroads : List[pedestrian_crossroad],
		                        bus_stops : List[bus_stop],
		                        tram_stops : List[tram_stop],
		                        zones : List[zone])
                            
   def dimensionsToJValue(dimensions : dimensions) : JValue = {
    return ("x" -> dimensions.x) ~ ("y" -> dimensions.y) 
   }
                            
   def pointToJValue(point : point) : JValue = {
     return ("x" -> point.x) ~ ("y" -> point.y) 
   }
  
  def beginToJValue(begin : begin) : JValue = {
    return ("point" -> pointToJValue(begin.point)) ~ ("id" -> begin.id)
  }
  
  def endToJValue(end : end) : JValue = {
    return ("point" -> pointToJValue(end.point)) ~ ("id" -> end.id)
  }
  
  def coordinatesToJValue(coordinates : coordinates) : JValue = {
    return ("begin" -> beginToJValue(coordinates.begin)) ~ ("end" -> endToJValue(coordinates.end))
  }
  
  def roadToJValue(road : road) : JValue = {
    val id = ("id" -> road.id)
    val coordinates = ("coordinates" -> coordinatesToJValue(road.coordinates))
    val lanesIDs = ("lanesIDs" -> road.lanesIDs)
    val zonesIDs = ("zonesIDs" -> road.zonesIDs)
    return id ~ coordinates ~ lanesIDs ~ zonesIDs
  }
  
  def laneToJValue(lane : lane) : JValue = {
    val id = ("id" -> lane.id)
    val road = ("road" -> lane.road)
    val beginToEnd = ("begintoend" -> lane.begintoend)
    val tram = ("tram" -> lane.tram)
    val bus_routes = ("bus_routes" -> lane.bus_routes)
    return id ~ road ~ beginToEnd ~ tram ~ bus_routes
  }
  
  def vertexToJValue(vertex : vertex) : JValue = {
    return ("point" -> pointToJValue(vertex.point)) ~ ("id" -> vertex.id)
  }
  
  def crossroadToJValue(crossroad : crossroad) : JValue = {
    val id = ("id" -> crossroad.id)
    val vertexes = ("vertexes" -> crossroad.vertexes.map { vertex => vertexToJValue(vertex) })
    val category = ("category" -> printCategory(crossroad.category))
    val extendable = ("extendable" -> crossroad.extendable)
    return id ~ vertexes ~ category ~ extendable
  }
  
  def pedestrianCrossroadToJValue(pedestrian_crossroad : pedestrian_crossroad) : JValue = {
    val id = ("id" -> pedestrian_crossroad.id)
    val coordinates = ("coordinates" -> coordinatesToJValue(pedestrian_crossroad.coordinates))
    return id ~ coordinates
  }
  
  def busStopToJValue(bus_stop : bus_stop) : JValue = {
    val id = ("id" -> bus_stop.id)
    val coordinates = ("coordinates" -> coordinatesToJValue(bus_stop.coordinates))
    val position = ("position" -> printPosition(bus_stop.position))
    val route = ("route" -> bus_stop.route)
    return id ~ coordinates ~ position ~ route
  }
  
  def tramStopToJValue(tram_stop : tram_stop) : JValue = {
    val id = ("id" -> tram_stop.id)
    val coordinates = ("coordinates" -> coordinatesToJValue(tram_stop.coordinates))
    val position = ("position" -> printPosition(tram_stop.position))
    val route = ("route" -> tram_stop.route)
    return id ~ coordinates ~ position ~ route
  }
  
  def zoneToJValue(zone : zone) : JValue = {
    val id = ("id" -> zone.id)
    val road = ("road" -> zone.road)
    val coordinates = ("coordinates" -> coordinatesToJValue(zone.coordinates))
    val variety = ("variety" -> printVariety(zone.variety))
    val position = ("position" -> printPosition(zone.position))
    return id ~ road ~ coordinates ~ variety ~ position
  }
  
  def urbanElementsToJValue(urban_elements : urban_elements) : JValue = {
    val dimensions = ("dimensions" -> dimensionsToJValue(urban_elements.dimensions))
    val roads = ("roads" -> urban_elements.roads.map { road => roadToJValue(road) })
    val lanes = ("lanes" -> urban_elements.lanes.map { lane => laneToJValue(lane) })
    val crossroads = ("crossroads" -> urban_elements.crossroads.map { crossroad => crossroadToJValue(crossroad) })
    val pedestrian_crossroads = ("pedestrian_crossroads" -> urban_elements.pedestrian_crossroads.map { pedestrian_crossroad => pedestrianCrossroadToJValue(pedestrian_crossroad) })
    val bus_stops = ("bus_stops" -> urban_elements.bus_stops.map { bus_stop => busStopToJValue(bus_stop) })
    val tram_stops = ("tram_stops" -> urban_elements.tram_stops.map { tram_stop => tramStopToJValue(tram_stop) })
    val zones = ("zones" -> urban_elements.zones.map { zone => zoneToJValue(zone) })
    return dimensions ~ roads ~ lanes ~ crossroads ~ pedestrian_crossroads ~ bus_stops ~ tram_stops ~ zones
  }
                              
   val path = getClass.getResource("/map.json").getPath
   
   // mappa corrente
   val current_map = readAll("map.json")
   
   // dati della mappa corrente
   val current_map_x = getDimensionsX(current_map)
   val current_map_y = getDimensionsY(current_map)
   
   // UTILITY
   // Distanza euclidea tra due punti
   def getDistance(start : point, end : point) : Double = {
     return Math.sqrt((start.x - end.x)^2 + (start.y - end.y)^2)
   }
   
   // UTILITY
   // dato un incorcio, restituisce una tabella descrittiva delle lane entranti nell'incrocio
   // ogni entry ha:
   // - key = id lane entrante
   // - value = (flag se è del tram, up/down/left/right, lista delle lanes a cui dare precedenza)
   def getCrossroadConfiguration(crossroadId : String) : Map[String, (Boolean, position, List[String])] = {
     // PRECONDIZIONE: ad invocare il metodo è un angolo classic, semaphore o roundabout
     // se l'angolo è classic, ha tre vertici, tutti e tre corrispondenti a strade
     // se l'angolo è semaphore o roundabout, ha quattro vertici, tutti corrispondenti a strade
     var finalMap = Map[String, (Boolean, position, List[String])]()
     val crossroad = JSONReader.getCrossroad(current_map, crossroadId).get
     // up
     val upVertex = getMostXVertex(crossroad.vertexes, `up`)
     // down
     val downVertex = getMostXVertex(crossroad.vertexes, `down`)
     // left
     val leftVertex = getMostXVertex(crossroad.vertexes, `left`)
     // right
     val rightVertex = getMostXVertex(crossroad.vertexes, `right`)
     // capiamo per prima cosa se l'angolo è a 3 o a 4
     if(crossroad.vertexes.length == 3) {
       // siamo sicuramente in un incrocio classic
       // capiamo qual'è il vertice null
       if(upVertex == null) {
         // sinistra e destra sono la strada principale, sotto è la strada che si immette
         var priority = List[String]()
         val leftLanes = getEnteringLanes(leftVertex, `left`)
         for(lane <- leftLanes) {
           priority = priority :+ lane._1
           val tuple = (lane._2, `left`, List[String]())
           finalMap = finalMap + (lane._1 -> tuple)
         }
         val rightLanes = getEnteringLanes(rightVertex, `right`)
         for(lane <- rightLanes) {
           priority = priority :+ lane._1
           val tuple = (lane._2, `right`, List[String]())
           finalMap = finalMap + (lane._1 -> tuple)
         }
         val downLanes = getEnteringLanes(downVertex, `down`)
         for(lane <- downLanes) {
           val tuple = (lane._2, `down`, priority)
           finalMap = finalMap + (lane._1 -> tuple)
         }
       }
       else if(downVertex == null){
         // sinistra e destra sono la strada principale, sopra c'è la strada che si immette
         var priority = List[String]()
         val leftLanes = getEnteringLanes(leftVertex, `left`)
         for(lane <- leftLanes) {
           priority = priority :+ lane._1
           val tuple = (lane._2, `left`, List[String]())
           finalMap = finalMap + (lane._1 -> tuple)
         }
         val rightLanes = getEnteringLanes(rightVertex, `right`)
         for(lane <- rightLanes) {
           priority = priority :+ lane._1
           val tuple = (lane._2, `right`, List[String]())
           finalMap = finalMap + (lane._1 -> tuple)
         }
         val upLanes = getEnteringLanes(upVertex, `up`)
         for(lane <- upLanes) {
           val tuple = (lane._2, `up`, priority)
           finalMap = finalMap + (lane._1 -> tuple)
         }
       }
       else if(leftVertex == null) {
         // sopra e sotto sono la strada principale, destra c'è la strada che si immette
         var priority = List[String]()
         val upLanes = getEnteringLanes(upVertex, `up`)
         for(lane <- upLanes) {
           priority = priority :+ lane._1
           val tuple = (lane._2, `up`, List[String]())
           finalMap = finalMap + (lane._1 -> tuple)
         }
         val downLanes = getEnteringLanes(downVertex, `down`)
         for(lane <- downLanes) {
           priority = priority :+ lane._1
           val tuple = (lane._2, `down`, List[String]())
           finalMap = finalMap + (lane._1 -> tuple)
         }
         val rightLanes = getEnteringLanes(rightVertex, `right`)
         for(lane <- rightLanes) {
           val tuple = (lane._2, `right`, priority)
           finalMap = finalMap + (lane._1 -> tuple)
         }
       }
       else {
         // sopra e sotto sono la strada principale, sinistra c'è la strada che si immette
         var priority = List[String]()
         val upLanes = getEnteringLanes(upVertex, `up`)
         for(lane <- upLanes) {
           priority = priority :+ lane._1
           val tuple = (lane._2, `up`, List[String]())
           finalMap = finalMap + (lane._1 -> tuple)
         }
         val downLanes = getEnteringLanes(downVertex, `down`)
         for(lane <- downLanes) {
           priority = priority :+ lane._1
           val tuple = (lane._2, `down`, List[String]())
           finalMap = finalMap + (lane._1 -> tuple)
         }
         val leftLanes = getEnteringLanes(leftVertex, `left`)
         for(lane <- leftLanes) {
           val tuple = (lane._2, `left`, priority)
           finalMap = finalMap + (lane._1 -> tuple)
         }
       }
     }
     else {
       crossroad.category match {
         case `semaphore` =>
           // il semaforo si regola sulla base della catena di corsie scritta qui
           val downLanes = getEnteringLanes(downVertex, `down`)
           var previousLane = downLanes(downLanes.length - 1)._1
           val leftLanes = getEnteringLanes(leftVertex, `left`)
           for(lane <- leftLanes) {
             val tuple = (lane._2, `left`, List[String](previousLane))
             finalMap = finalMap + (lane._1 -> tuple)
             previousLane = lane._1
           }
           val rightLanes = getEnteringLanes(rightVertex, `right`)
           for(lane <- rightLanes) {
             val tuple = (lane._2, `right`, List[String](previousLane))
             finalMap = finalMap + (lane._1 -> tuple)
             previousLane = lane._1
           }
           val upLanes = getEnteringLanes(downVertex, `up`)
           for(lane <- upLanes) {
             val tuple = (lane._2, `up`, List[String](previousLane))
             finalMap = finalMap + (lane._1 -> tuple)
             previousLane = lane._1
           }
           for(lane <- downLanes) {
             val tuple = (lane._2, `down`, List[String](previousLane))
             finalMap = finalMap + (lane._1 -> tuple)
             previousLane = lane._1
           }
           
         case `roundabout` =>
           // ciascuno ha la lane alla propria sinistra come precedenza
           // dobbiamo capire se abbiamo una lane del tram oppure no (al massimo ve ne è una)
           val leftLanes = getEnteringLanes(leftVertex, `left`)
           val rightLanes = getEnteringLanes(rightVertex, `right`)
           val upLanes = getEnteringLanes(downVertex, `up`)
           val downLanes = getEnteringLanes(downVertex, `down`)
           if(leftLanes.length > 1) {
             // due corsie a sinistra, quella del tram è la più bassa
             var normal = 0
             var tram = 1
             if(leftLanes(0)._2 == true) {
               // la prima è del tram, la seconda è normale
               normal = 1
               tram = 0
             }
             val leftTupleUp = (leftLanes(normal)._2, `left`, List[String](upLanes(0)._1))
             finalMap = finalMap + (leftLanes(normal)._1 -> leftTupleUp)
             val leftTupleLow = (leftLanes(tram)._2, `left`, List[String](leftLanes(normal)._1))
             finalMap = finalMap + (leftLanes(tram)._1 -> leftTupleLow)
             val upTuple = (upLanes(0)._2, `up`, List[String](rightLanes(0)._1))
             finalMap = finalMap + (upLanes(0)._1 -> upTuple)
             val rightTuple = (rightLanes(0)._2, `right`, List[String](downLanes(0)._1))
             finalMap = finalMap + (rightLanes(0)._1 -> rightTuple)
             val downTuple = (downLanes(0)._2, `down`, List[String](leftLanes(tram)._1))
             finalMap = finalMap + (downLanes(0)._1 -> downTuple)
           }
           else if(rightLanes.length > 1) {
             // due corsie a destra, quella del tram è la più alta
             var normal = 0
             var tram = 1
             if(rightLanes(0)._2 == true) {
               // la prima è del tram, la seconda è normale
               normal = 1
               tram = 0
             }
             val leftTuple = (leftLanes(0)._2, `left`, List[String](upLanes(0)._1))
             finalMap = finalMap + (leftLanes(0)._1 -> leftTuple)
             val upTuple = (upLanes(0)._2, `up`, List[String](rightLanes(tram)._1))
             finalMap = finalMap + (upLanes(0)._1 -> upTuple)
             val rightTupleUp = (rightLanes(tram)._2, `right`, List[String](rightLanes(normal)._1))
             finalMap = finalMap + (rightLanes(tram)._1 -> rightTupleUp)
             val rightTupleDown = (rightLanes(normal)._2, `right`, List[String](downLanes(0)._1))
             finalMap = finalMap + (rightLanes(normal)._1 -> rightTupleDown)
             val downTuple = (downLanes(0)._2, `down`, List[String](leftLanes(0)._1))
             finalMap = finalMap + (downLanes(0)._1 -> downTuple)
           }
           else if(upLanes.length > 1) {
             // due corsie in alto, quella del tram è la più a sx
             var normal = 0
             var tram = 1
             if(upLanes(0)._2 == true) {
               // la prima è del tram, la seconda è normale
               normal = 1
               tram = 0
             }
             val leftTuple = (leftLanes(0)._2, `left`, List[String](upLanes(tram)._1))
             finalMap = finalMap + (leftLanes(0)._1 -> leftTuple)
             val upTupleLeft = (upLanes(tram)._2, `up`, List[String](upLanes(normal)._1))
             finalMap = finalMap + (upLanes(tram)._1 -> upTupleLeft)
             val upTupleRight = (upLanes(normal)._2, `up`, List[String](rightLanes(0)._1))
             finalMap = finalMap + (upLanes(normal)._1 -> upTupleRight)
             val rightTuple = (rightLanes(0)._2, `right`, List[String](downLanes(0)._1))
             finalMap = finalMap + (rightLanes(0)._1 -> rightTuple)
             val downTuple = (downLanes(0)._2, `down`, List[String](leftLanes(0)._1))
             finalMap = finalMap + (downLanes(0)._1 -> downTuple)
           }
           else if(downLanes.length > 1) {
             // due corsie in basso, quella del tram è la più a dx
             var normal = 0
             var tram = 1
             if(downLanes(0)._2 == true) {
               // la prima è del tram, la seconda è normale
               normal = 1
               tram = 0
             }
             val leftTuple = (leftLanes(0)._2, `left`, List[String](upLanes(0)._1))
             finalMap = finalMap + (leftLanes(0)._1 -> leftTuple)
             val upTuple = (upLanes(0)._2, `up`, List[String](rightLanes(0)._1))
             finalMap = finalMap + (upLanes(0)._1 -> upTuple)
             val rightTuple = (rightLanes(0)._2, `right`, List[String](downLanes(tram)._1))
             finalMap = finalMap + (rightLanes(0)._1 -> rightTuple)
             val downTupleRight = (downLanes(tram)._2, `down`, List[String](downLanes(normal)._1))
             finalMap = finalMap + (downLanes(tram)._1 -> downTupleRight)
             val downTupleLeft = (downLanes(normal)._2, `down`, List[String](leftLanes(0)._1))
             finalMap = finalMap + (downLanes(normal)._1 -> downTupleLeft)
           }
           else {
             // non ci sono corsie del tram, quindi ogni sequenza di lanes è in realtà una singola lane
             val leftTuple = (leftLanes(0)._2, `left`, List[String](upLanes(0)._1))
             finalMap = finalMap + (leftLanes(0)._1 -> leftTuple)
             val upTuple = (upLanes(0)._2, `up`, List[String](rightLanes(0)._1))
             finalMap = finalMap + (upLanes(0)._1 -> upTuple)
             val rightTuple = (rightLanes(0)._2, `right`, List[String](downLanes(0)._1))
             finalMap = finalMap + (rightLanes(0)._1 -> rightTuple)
             val downTuple = (downLanes(0)._2, `down`, List[String](leftLanes(0)._1))
             finalMap = finalMap + (downLanes(0)._1 -> downTuple)
           }
       }
     }
     return finalMap
   }
   
   // UTILITY
   // da una lista di vertici, ritorna quello più rispetto alla direzione indicata
   def getMostXVertex(vertexes : List[vertex], position : position) : vertex = {
     position match {
       case `up` =>
         // cerchiamo il punto con coordinata y più alta
         var best = vertexes(0)
         // segnala se i punti migliori sono due
         var bestFlag = false
         for(index <- 1 to vertexes.length - 1) {
           if(best.point.y < vertexes(index).point.y) {
             best = vertexes(index)
             bestFlag = false
           }
           else if(best.point.y == vertexes(index).point.y) {
             bestFlag = true
           }
         }
         if(bestFlag) {
           // significa che siamo con 3 vertici e due sono in alto
           return null
         }
         else {
           return best
         }
       case `down` =>
         // cerchiamo il punto con coordinata y più bassa
         var best = vertexes(0)
         // segnala se i punti migliori sono due
         var bestFlag = false
         for(index <- 1 to vertexes.length - 1) {
           if(best.point.y > vertexes(index).point.y) {
             best = vertexes(index)
             bestFlag = false
           }
           else if(best.point.y == vertexes(index).point.y) {
             bestFlag = true
           }
         }
         if(bestFlag) {
           // significa che siamo con 3 vertici e due sono in alto
           return null
         }
         else {
           return best
         }
       case `left` =>
         // cerchiamo il punto con coordinata x più bassa
         var best = vertexes(0)
         // segnala se i punti migliori sono due
         var bestFlag = false
         for(index <- 1 to vertexes.length - 1) {
           if(best.point.x > vertexes(index).point.x) {
             best = vertexes(index)
             bestFlag = false
           }
           else if(best.point.x == vertexes(index).point.x) {
             bestFlag = true
           }
         }
         if(bestFlag) {
           // significa che siamo con 3 vertici e due sono in alto
           return null
         }
         else {
           return best
         }
       case `right` =>
         // cerchiamo il punto con coordinata x più alta
         var best = vertexes(0)
         // segnala se i punti migliori sono due
         var bestFlag = false
         for(index <- 1 to vertexes.length - 1) {
           if(best.point.x < vertexes(index).point.x) {
             best = vertexes(index)
             bestFlag = false
           }
           else if(best.point.x == vertexes(index).point.x) {
             bestFlag = true
           }
         }
         if(bestFlag) {
           // significa che siamo con 3 vertici e due sono in alto
           return null
         }
         else {
           return best
         }
     }
   }
   
   // UTILITY
   // dato un vertice e una direzione, recupera le lane entranti corrispondenti, segnalando se sono del tram o meno
   def getEnteringLanes(vertex : vertex, position : position) : List[(String, Boolean)] = {
     // ci prendiamo cura del fatto che possa essere confinante con un incrocio angle
     var desiredLanes = List[(String, Boolean)]()
     if(JSONReader.getRoad(current_map, vertex.id).isEmpty == false) {
       val road = JSONReader.getRoad(current_map, vertex.id).get
       position match {
         case `up` | `right` =>
           // le lanes entranti hanno beginToEnd = false
           for(laneId <- road.lanesIDs) {
             val currentLane = JSONReader.getLane(current_map, laneId).get
             if(currentLane.begintoend == false) {
               val tuple = (laneId, currentLane.tram)
               desiredLanes = desiredLanes :+ tuple
             }
           }
         case `down` | `left` =>
           // le lanes entranti hanno beginToEnd = true
           for(laneId <- road.lanesIDs) {
             val currentLane = JSONReader.getLane(current_map, laneId).get
             if(currentLane.begintoend == true) {
               val tuple = (laneId, currentLane.tram)
               desiredLanes = desiredLanes :+ tuple
             }
           }
       }
     }
     else {
       // il vicino deve essere un incrocio, e deve essere angle
       assert(JSONReader.getCrossroad(current_map, vertex.id).isEmpty == false)
       val crossroad = JSONReader.getCrossroad(current_map, vertex.id).get
       assert(crossroad.category == `angle`)
       // guarda il vertice nil per capire la direzione del lato
       var nilVertex : vertex = null
       for(current <- crossroad.vertexes) {
         if(current.id == "nil") {
           nilVertex = current
         }
       }
       // trova anche l'altro vertice
       var remainingVertex : vertex = null
       for(current <- crossroad.vertexes) {
         if(current.id != "nil" && !(current.point.x == vertex.point.x && current.point.y == vertex.point.y)) {
           remainingVertex = current
         }
       }
       assert(nilVertex != null && remainingVertex != null)
       val road = JSONReader.getRoad(current_map, remainingVertex.id).get
       if(vertex.point.x < nilVertex.point.x && vertex.point.y == nilVertex.point.y) {
         // destra
         // la y dell'altro vertice o è maggiore o è minore di nilVertex
         if(remainingVertex.point.y > nilVertex.point.y) {
           // alto
           for(laneId <- road.lanesIDs) {
             val currentLane = JSONReader.getLane(current_map, laneId).get
             if(currentLane.begintoend == false) {
               val tuple = (laneId, currentLane.tram)
               desiredLanes = desiredLanes :+ tuple
             }
           }
         }
         else {
           // basso
           for(laneId <- road.lanesIDs) {
             val currentLane = JSONReader.getLane(current_map, laneId).get
             if(currentLane.begintoend == true) {
               val tuple = (laneId, currentLane.tram)
               desiredLanes = desiredLanes :+ tuple
             }
           }
         }
       }
       else if(vertex.point.x > nilVertex.point.x && vertex.point.y == nilVertex.point.y) {
         // sinistra
         // la y dell'altro vertice o è maggiore o è minore di nilVertex
         if(remainingVertex.point.y > nilVertex.point.y) {
           // alto
           for(laneId <- road.lanesIDs) {
             val currentLane = JSONReader.getLane(current_map, laneId).get
             if(currentLane.begintoend == false) {
               val tuple = (laneId, currentLane.tram)
               desiredLanes = desiredLanes :+ tuple
             }
           }
         }
         else {
           // basso
           for(laneId <- road.lanesIDs) {
             val currentLane = JSONReader.getLane(current_map, laneId).get
             if(currentLane.begintoend == true) {
               val tuple = (laneId, currentLane.tram)
               desiredLanes = desiredLanes :+ tuple
             }
           }
         }
       }
       else if(vertex.point.y < nilVertex.point.y && vertex.point.x == nilVertex.point.x) {
         // su
         // la x dell'altro vertice o è maggiore o è minore di nilVertex
         if(remainingVertex.point.x > nilVertex.point.x) {
           // destra
           for(laneId <- road.lanesIDs) {
             val currentLane = JSONReader.getLane(current_map, laneId).get
             if(currentLane.begintoend == false) {
               val tuple = (laneId, currentLane.tram)
               desiredLanes = desiredLanes :+ tuple
             }
           }
         }
         else {
           // sinistra
           for(laneId <- road.lanesIDs) {
             val currentLane = JSONReader.getLane(current_map, laneId).get
             if(currentLane.begintoend == true) {
               val tuple = (laneId, currentLane.tram)
               desiredLanes = desiredLanes :+ tuple
             }
           }
         }
       }
       else {
         // giù
         // la x dell'altro vertice o è maggiore o è minore di nilVertex
         if(remainingVertex.point.x > nilVertex.point.x) {
           // destra
           for(laneId <- road.lanesIDs) {
             val currentLane = JSONReader.getLane(current_map, laneId).get
             if(currentLane.begintoend == false) {
               val tuple = (laneId, currentLane.tram)
               desiredLanes = desiredLanes :+ tuple
             }
           }
         }
         else {
           // sinistra
           for(laneId <- road.lanesIDs) {
             val currentLane = JSONReader.getLane(current_map, laneId).get
             if(currentLane.begintoend == true) {
               val tuple = (laneId, currentLane.tram)
               desiredLanes = desiredLanes :+ tuple
             }
           }
         }
       }
     }
     return desiredLanes
   }
   
   // UTILITY
   // dato un incrocio confinante con un'altro, restituisce tra i due quello classic
   def getClassicCrossroad(input : crossroad) : crossroad = {
     // PRECONDIZIONE: gli incroci doppi sono sempre composti da un incrocio angle e uno classic
     if(input.category == `classic`) {
       return input
     }
     else {
       // guarda i vicini dell'incrocio in input, che deve essere angle
       assert(input.category == `angle`)
       var output : crossroad = null
       for(vertex <- input.vertexes) {
         val current = JSONReader.getCrossroad(current_map, vertex.id)
         if(current.isEmpty == false) {
           output = current.get
         }
       }
       assert(output.category == `classic`)
       return output
     }
   }

}