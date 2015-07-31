import org.json4s._
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods._
import java.io._

import scala.util.Sorting

import Domain._
import Domain.category
import Domain.position
import Domain.variety
import JSONUtilities._

object JSONReader {

	// necessario per corretto parsing
	implicit lazy val formats = DefaultFormats + new org.json4s.ext.EnumNameSerializer(category) + new org.json4s.ext.EnumNameSerializer(position) +new org.json4s.ext.EnumNameSerializer(variety)

	def readAll(name : String) : urban_elements = {
		val source = scala.io.Source.fromFile(new File(path.replace("map.json", name)))
		val environmentString = try source.getLines mkString finally source.close()
		parse(environmentString, false).extract[urban_elements]
	}

	// metodi getAll

	def getAllRoads(environment : urban_elements) : List[road] = {
		environment match {
		case (urban_elements(roads,_,_,_,_,_,_)) =>
		return roads
		}
	}

	def getAllLanes(environment : urban_elements) : List[lane] = {
		environment match {
		case (urban_elements(_,lanes,_,_,_,_,_)) =>
		return lanes
		}
	}

	def getAllCrossroads(environment : urban_elements) : List[crossroad] = {
			environment match {
			case (urban_elements(_,_,crossroads,_,_,_,_)) =>
			return crossroads
			}
	}

	def getAllPedestrianCrossroads(environment : urban_elements) : List[pedestrian_crossroad] = {
			environment match {
			case (urban_elements(_,_,_,pedestrian_crossroads,_,_,_)) =>
			return pedestrian_crossroads
			}
	}

	def getAllBusStops(environment : urban_elements) : List[bus_stop] = {
			environment match {
			case (urban_elements(_,_,_,_,bus_stops,_,_)) =>
			return bus_stops
			}
	}

	def getAllTramStops(environment : urban_elements) : List[tram_stop] = {
			environment match {
			case (urban_elements(_,_,_,_,_,tram_stops,_)) =>
			return tram_stops
			}
	}

	def getAllZones(environment : urban_elements) : List[zone] = {
			environment match {
			case (urban_elements(_,_,_,_,_,_,zones)) =>
			return zones
			}
	}

	// metodi get per road
  
  def getRoad(environment : urban_elements, id : String) : Option[road] = {
    environment match {
    case (urban_elements(roads,_,_,_,_,_,_)) =>
    return roads.find { road => road.id == id }
    }
  }

	def getRoadBeginPoint(environment : urban_elements, id : String) : (Int,Int) = {
		environment match {
		case (urban_elements(roads,_,_,_,_,_,_)) =>
		val road = roads.find { road => road.id == id }
		if(road.equals(None)) {
			return (-1,-1)
		}
		else {
			return (road.get.coordinates.begin.point.x, road.get.coordinates.begin.point.y)
		}
		}
	}

	def getRoadEndPoint(environment : urban_elements, id : String) : (Int,Int) = {
		environment match {
		case (urban_elements(roads,_,_,_,_,_,_)) =>
		val road = roads.find { road => road.id == id }
		if(road.equals(None)) {
			return (-1,-1)
		}
		else {
			return (road.get.coordinates.end.point.x, road.get.coordinates.end.point.y)
		}
		}
	}

	def getRoadBeginNeighbor(environment : urban_elements, id : String) : String = {
		environment match {
		case (urban_elements(roads,_,_,_,_,_,_)) =>
		val road = roads.find { road => road.id == id }
		if(road.equals(None)) {
			return ""
		}
		else {
			return road.get.coordinates.begin.id
		}
		}
	}

	def getRoadEndNeighbor(environment : urban_elements, id : String) : String = {
		environment match {
		case (urban_elements(roads,_,_,_,_,_,_)) =>
		val road = roads.find { road => road.id == id }
		if(road.equals(None)) {
			return ""
		}
		else {
			return road.get.coordinates.end.id
		}
		}
	}

	def getRoadLanes(environment : urban_elements, id : String) : List[String] = {
		environment match {
		case (urban_elements(roads,_,_,_,_,_,_)) =>
		val road = roads.find { road => road.id == id }
		if(road.equals(None)) {
			return List[String]()
		}
		else {
			return road.get.lanesIDs
		}
		}
	}

	def getRoadZones(environment : urban_elements, id : String) : List[String] = {
		environment match {
		case (urban_elements(roads,_,_,_,_,_,_)) =>
		val road = roads.find { road => road.id == id }
		if(road.equals(None)) {
			return List[String]()
		}
		else {
			return road.get.zonesIDs
		}
		}
	}

	// metodi get per lane
  
  def getLane(environment : urban_elements, id : String) : Option[lane] = {
    environment match {
    case (urban_elements(_,lanes,_,_,_,_,_)) =>
    return lanes.find { lane => lane.id == id }
    }
  }

	def getLaneRoad(environment : urban_elements, id : String) : String = {
		environment match {
		case (urban_elements(_,lanes,_,_,_,_,_)) =>
		val lane = lanes.find { lane => lane.id == id }
		if(lane.equals(None)) {
			return ""
		}
		else {
			return lane.get.road
		}
		}
	}

	def getLaneBeginToEnd(environment : urban_elements, id : String) : Boolean = {
		environment match {
		case (urban_elements(_,lanes,_,_,_,_,_)) =>
		val lane = lanes.find { lane => lane.id == id }
		if(lane.equals(None)) {
			return false
		}
		else {
			return lane.get.begintoend
		}
		}
	}

	def getLaneTram(environment : urban_elements, id : String) : Boolean = {
		environment match {
		case (urban_elements(_,lanes,_,_,_,_,_)) =>
		val lane = lanes.find { lane => lane.id == id }
		if(lane.equals(None)) {
			return false
		}
		else {
			return lane.get.tram
		}
		}
	}

	// metodi get per crossroad
  
  def getCrossroad(environment : urban_elements, id : String) : Option[crossroad] = {
    environment match {
    case (urban_elements(_,_,crossroads,_,_,_,_)) =>
    return crossroads.find { crossroad => crossroad.id == id }
    }
  }

	def getCrossroadNumberOfVertex(environment : urban_elements, id : String) : Int = {
		environment match {
		case (urban_elements(_,_,crossroads,_,_,_,_)) =>
		val crossroad = crossroads.find { crossroad => crossroad.id == id }
		if(crossroad.equals(None)) {
			return -1
		}
		else {
			return crossroad.get.vertexes.length
		}
		}
	}

	def getCrossroadVertexCoordinates(environment : urban_elements, id : String, index : Int) : (Int,Int) = {
		environment match {
		case (urban_elements(_,_,crossroads,_,_,_,_)) =>
		val crossroad = crossroads.find { crossroad => crossroad.id == id }
		if(crossroad.equals(None)) {
			return (-1,-1)
		}
		else {
			if(crossroad.get.vertexes.length <= index) {
				return (-1,-1)
			}
			else {
				return (crossroad.get.vertexes(index).point.x, crossroad.get.vertexes(index).point.y)
			}
		}
		}
	}

	def getCrossroadVertexNeighbor(environment : urban_elements, id : String, index : Int) : String = {
		environment match {
		case (urban_elements(_,_,crossroads,_,_,_,_)) =>
		val crossroad = crossroads.find { crossroad => crossroad.id == id }
		if(crossroad.equals(None)) {
			return ""
		}
		else {
			if(crossroad.get.vertexes.length <= index) {
				return ""
			}
			else {
				return crossroad.get.vertexes(index).id
			}
		}
		}
	}

	def getCrossroadCategory(environment : urban_elements, id : String) : String = {
		environment match {
		case (urban_elements(_,_,crossroads,_,_,_,_)) =>
		val crossroad = crossroads.find { crossroad => crossroad.id == id }
		if(crossroad.equals(None)) {
			return ""
		}
		else {
			crossroad.get.category match {
			case category.nil => return "nil"
			case category.angle => return "angle"
			case category.classic => return "classic"
			case category.semaphore => return "semaphore"
			case category.roundabout => return "roundabout"
			}
		}
		}
	}

	def getCrossroadExtendable(environment : urban_elements, id : String) : Boolean = {
		environment match {
		case (urban_elements(_,_,crossroads,_,_,_,_)) =>
		val crossroad = crossroads.find { crossroad => crossroad.id == id }
		if(crossroad.equals(None)) {
			return false
		}
		else {
			return crossroad.get.extendable
		}
		}
	}

	// metodi get per pedestrian_crossroad
  
  def getPedestrianCrossroad(environment : urban_elements, id : String) : Option[pedestrian_crossroad] = {
    environment match {
    case (urban_elements(_,_,_,pedestrian_crossroads,_,_,_)) =>
    return pedestrian_crossroads.find { pedestrian_crossroad => pedestrian_crossroad.id == id }
    }
  }

	def getPedestrianCrossroadBeginPoint(environment : urban_elements, id : String) : (Int,Int) = {
		environment match {
		case (urban_elements(_,_,_,pedestrian_crossroads,_,_,_)) =>
		val pedestrian_crossroad = pedestrian_crossroads.find { pedestrian_crossroad => pedestrian_crossroad.id == id }
		if(pedestrian_crossroad.equals(None)) {
			return (-1,-1)
		}
		else {
			return (pedestrian_crossroad.get.coordinates.begin.point.x, pedestrian_crossroad.get.coordinates.begin.point.y)
		}
		}
	}

	def getPedestrianCrossroadEndPoint(environment : urban_elements, id : String) : (Int,Int) = {
		environment match {
		case (urban_elements(_,_,_,pedestrian_crossroads,_,_,_)) =>
		val pedestrian_crossroad = pedestrian_crossroads.find { pedestrian_crossroad => pedestrian_crossroad.id == id }
		if(pedestrian_crossroad.equals(None)) {
			return (-1,-1)
		}
		else {
			return (pedestrian_crossroad.get.coordinates.end.point.x, pedestrian_crossroad.get.coordinates.end.point.y)
		}
		}
	}

	def getPedestrianCrossroadBeginNeighbor(environment : urban_elements, id : String) : String = {
		environment match {
		case (urban_elements(_,_,_,pedestrian_crossroads,_,_,_)) =>
		val pedestrian_crossroad = pedestrian_crossroads.find { pedestrian_crossroad => pedestrian_crossroad.id == id }
		if(pedestrian_crossroad.equals(None)) {
			return ""
		}
		else {
			return pedestrian_crossroad.get.coordinates.begin.id
		}
		}
	}

	def getPedestrianCrossroadEndNeighbor(environment : urban_elements, id : String) : String = {
		environment match {
		case (urban_elements(_,_,_,pedestrian_crossroads,_,_,_)) =>
		val pedestrian_crossroad = pedestrian_crossroads.find { pedestrian_crossroad => pedestrian_crossroad.id == id }
		if(pedestrian_crossroad.equals(None)) {
			return ""
		}
		else {
			return pedestrian_crossroad.get.coordinates.end.id
		}
		}
	}

	// metodi get per bus_stop
  
  def getBusStop(environment : urban_elements, id : String) : Option[bus_stop] = {
    environment match {
    case (urban_elements(_,_,_,_,bus_stops,_,_)) =>
    return bus_stops.find { bus_stop => bus_stop.id == id }
    }
  }

	def getBusStopBeginPoint(environment : urban_elements, id : String) : (Int,Int) = {
		environment match {
		case (urban_elements(_,_,_,_,bus_stops,_,_)) =>
		val bus_stop = bus_stops.find { bus_stop => bus_stop.id == id }
		if(bus_stop.equals(None)) {
			return (-1,-1)
		}
		else {
			return (bus_stop.get.coordinates.begin.point.x, bus_stop.get.coordinates.begin.point.y)
		}
		}
	}

	def getBusStopEndPoint(environment : urban_elements, id : String) : (Int,Int) = {
		environment match {
		case (urban_elements(_,_,_,_,bus_stops,_,_)) =>
		val bus_stop = bus_stops.find { bus_stop => bus_stop.id == id }
		if(bus_stop.equals(None)) {
			return (-1,-1)
		}
		else {
			return (bus_stop.get.coordinates.end.point.x, bus_stop.get.coordinates.end.point.y)
		}
		}
	}

	def getBusStopBeginNeighbor(environment : urban_elements, id : String) : String = {
		environment match {
		case (urban_elements(_,_,_,_,bus_stops,_,_)) =>
		val bus_stop = bus_stops.find { bus_stop => bus_stop.id == id }
		if(bus_stop.equals(None)) {
			return ""
		}
		else {
			return bus_stop.get.coordinates.begin.id
		}
		}
	}

	def getBusStopEndNeighbor(environment : urban_elements, id : String) : String = {
		environment match {
		case (urban_elements(_,_,_,_,bus_stops,_,_)) =>
		val bus_stop = bus_stops.find { bus_stop => bus_stop.id == id }
		if(bus_stop.equals(None)) {
			return ""
		}
		else {
			return bus_stop.get.coordinates.end.id
		}
		}
	}

	def getBusStopPosition(environment : urban_elements, id : String) : String = {
		environment match {
		case (urban_elements(_,_,_,_,bus_stops,_,_)) =>
		val bus_stop = bus_stops.find { bus_stop => bus_stop.id == id }
		if(bus_stop.equals(None)) {
			return ""
		}
		else {
			bus_stop.get.position match {
			case position.up => return "up"
			case position.down => return "down"
			case position.left => return "left"
			case position.right => return "right"
			}
		}
		}
	}

	def getBusStopRoute(environment : urban_elements, id : String) : Int = {
		environment match {
		case (urban_elements(_,_,_,_,bus_stops,_,_)) =>
		val bus_stop = bus_stops.find { bus_stop => bus_stop.id == id }
		if(bus_stop.equals(None)) {
			return -1
		}
		else {
			return bus_stop.get.route
		}
		}
	}

	// metodi get per tram_stop
  
  def getTram(environment : urban_elements, id : String) : Option[tram_stop] = {
    environment match {
    case (urban_elements(_,_,_,_,_,tram_stops,_)) =>
    return tram_stops.find { tram_stop => tram_stop.id == id }
    }
  }

	def getTramStopBeginPoint(environment : urban_elements, id : String) : (Int,Int) = {
		environment match {
		case (urban_elements(_,_,_,_,_,tram_stops,_)) =>
		val tram_stop = tram_stops.find { tram_stop => tram_stop.id == id }
		if(tram_stop.equals(None)) {
			return (-1,-1)
		}
		else {
			return (tram_stop.get.coordinates.begin.point.x, tram_stop.get.coordinates.begin.point.y)
		}
		}
	}

	def getTramStopEndPoint(environment : urban_elements, id : String) : (Int,Int) = {
		environment match {
		case (urban_elements(_,_,_,_,_,tram_stops,_)) =>
		val tram_stop = tram_stops.find { tram_stop => tram_stop.id == id }
		if(tram_stop.equals(None)) {
			return (-1,-1)
		}
		else {
			return (tram_stop.get.coordinates.end.point.x, tram_stop.get.coordinates.end.point.y)
		}
		}
	}

	def getTramStopBeginNeighbor(environment : urban_elements, id : String) : String = {
		environment match {
		case (urban_elements(_,_,_,_,_,tram_stops,_)) =>
		val tram_stop = tram_stops.find { tram_stop => tram_stop.id == id }
		if(tram_stop.equals(None)) {
			return ""
		}
		else {
			return tram_stop.get.coordinates.begin.id
		}
		}
	}

	def getTramStopEndNeighbor(environment : urban_elements, id : String) : String = {
		environment match {
		case (urban_elements(_,_,_,_,_,tram_stops,_)) =>
		val tram_stop = tram_stops.find { tram_stop => tram_stop.id == id }
		if(tram_stop.equals(None)) {
			return ""
		}
		else {
			return tram_stop.get.coordinates.end.id
		}
		}
	}

	def getTramStopPosition(environment : urban_elements, id : String) : String = {
		environment match {
		case (urban_elements(_,_,_,_,_,tram_stops,_)) =>
		val tram_stop = tram_stops.find { tram_stop => tram_stop.id == id }
		if(tram_stop.equals(None)) {
			return ""
		}
		else {
			tram_stop.get.position match {
			case position.up => return "up"
			case position.down => return "down"
			case position.left => return "left"
			case position.right => return "right"
			}
		}
		}
	}

	def getTramStopRoute(environment : urban_elements, id : String) : Int = {
		environment match {
		case (urban_elements(_,_,_,_,_,tram_stops,_)) =>
		val tram_stop = tram_stops.find { tram_stop => tram_stop.id == id }
		if(tram_stop.equals(None)) {
			return -1
		}
		else {
			return tram_stop.get.route
		}
		}
	}

	// metodi get per zone
  
  def getZone(environment : urban_elements, id : String) : Option[zone] = {
    environment match {
      case (urban_elements(_,_,_,_,_,_,zones)) =>
        return zones.find { zone => zone.id == id }
    }
  }

	def getZoneRoad(environment : urban_elements, id : String) : String = {
		environment match {
		case (urban_elements(_,_,_,_,_,_,zones)) =>
		val zone = zones.find { zone => zone.id == id }
		if(zone.equals(None)) {
			return ""
		}
		else {
			return zone.get.road
		}
		}
	}

	def getZoneBeginPoint(environment : urban_elements, id : String) : (Int,Int) = {
		environment match {
		case (urban_elements(_,_,_,_,_,_,zones)) =>
		val zone = zones.find { zone => zone.id == id }
		if(zone.equals(None)) {
			return (-1,-1)
		}
		else {
			return (zone.get.coordinates.begin.point.x, zone.get.coordinates.begin.point.y)
		}
		}
	}

	def getZoneEndPoint(environment : urban_elements, id : String) : (Int,Int) = {
		environment match {
		case (urban_elements(_,_,_,_,_,_,zones)) =>
		val zone = zones.find { zone => zone.id == id }
		if(zone.equals(None)) {
			return (-1,-1)
		}
		else {
			return (zone.get.coordinates.end.point.x, zone.get.coordinates.end.point.y)
		}
		}
	}

	def getZoneBeginNeighbor(environment : urban_elements, id : String) : String = {
		environment match {
		case (urban_elements(_,_,_,_,_,_,zones)) =>
		val zone = zones.find { zone => zone.id == id }
		if(zone.equals(None)) {
			return ""
		}
		else {
			return zone.get.coordinates.begin.id
		}
		}
	}

	def getZoneEndNeighbor(environment : urban_elements, id : String) : String = {
		environment match {
		case (urban_elements(_,_,_,_,_,_,zones)) =>
		val zone = zones.find { zone => zone.id == id }
		if(zone.equals(None)) {
			return ""
		}
		else {
			return zone.get.coordinates.end.id
		}
		}
	}

	def getZoneVariety(environment : urban_elements, id : String) : String = {
		environment match {
		case (urban_elements(_,_,_,_,_,_,zones)) =>
		val zone = zones.find { zone => zone.id == id }
		if(zone.equals(None)) {
			return ""
		}
		else {
			zone.get.variety match {
			case variety.houseplace => return "houseplace"
			case variety.workplace => return "workplace"
			case variety.funplace => return "funplace"
			}
		}
		}
	}

	def getZonePosition(environment : urban_elements, id : String) : String = {
		environment match {
		case (urban_elements(_,_,_,_,_,_,zones)) =>
		val zone = zones.find { zone => zone.id == id }
		if(zone.equals(None)) {
			return ""
		}
		else {
			zone.get.position match {
			case position.up => return "up"
			case position.down => return "down"
			case position.left => return "left"
			case position.right => return "right"
			}
		}
		}
	}
  
  // metodi di utilità
  
  case class extendable(id : String, x : Int, y : Int)
  
  def get_mostExtendableCrossroads(environment : urban_elements, side : String) : (String,String) = {
    environment match {
      case (urban_elements(_,_,crossroads,_,_,_,_)) =>
        var extendableList = List[extendable]()
        for(crossroad <- crossroads) {
          if(crossroad.extendable == true) {
            val (_, x, y, _) = splitId(crossroad.id)
            extendableList = extendableList :+ extendable(crossroad.id, x, y)
          }
        }
        side match {
          case "left" =>
            val ordered = extendableList.sortBy(u => (u.x, u.y))
            return (ordered(0).id, ordered(1).id)
          case "right" =>
            val ordered = extendableList.sortBy(u => (u.x, u.y)).reverse
            return (ordered(0).id, ordered(1).id)
          case "down" =>
            val ordered = extendableList.sortBy(u => (u.y, u.x))
            return (ordered(0).id, ordered(1).id)
          case "up" =>
            val ordered = extendableList.sortBy(u => (u.y, u.x)).reverse
            return (ordered(0).id, ordered(1).id)
        }
    }
  }

}