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
   
   // dati della mappa corrente
   val current_map_x = getDimensionsX(current_map)
   val current_map_y = getDimensionsY(current_map)
   
   // mappa corrente
   lazy val current_map = readAll("map.json")
   
   // UTILITY
   // Distanza euclidea tra due punti
   def getDistance(start : point, end : point) : Double = {
     return Math.sqrt((start.x - end.x)^2 + (start.y - end.y)^2)
   }

}