import org.json4s._
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods._
import org.json4s.JsonDSL._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{ write, writePretty }
import java.io._
import scalax.io._

import Domain._
import Domain.category
import Domain.position
import Domain.variety

object JSONWriter {

	// necessario per la corretta scrittura
	implicit lazy val formats =  Serialization.formats(
			ShortTypeHints(
					List(
							classOf[point],
							classOf[begin],
							classOf[end],
							classOf[coordinates],
							classOf[road],
							classOf[lane],
							classOf[vertex],
							classOf[crossroad],
							classOf[pedestrian_crossroad],
							classOf[bus_stop],
							classOf[tram_stop],
							classOf[zone],
							classOf[urban_elements]
							)
					)
			)

	def addRoad(environment : urban_elements, road : road) : urban_elements = {
		environment match {
		case (urban_elements(roads,lanes,crossroads,pedestrian_crossroads,bus_stops,tram_stops,zones)) =>
		return urban_elements(roads :+ road,
				lanes,
				crossroads, 
				pedestrian_crossroads,
				bus_stops,
				tram_stops,
				zones)
		}
	}

	def addLane(environment : urban_elements, lane : lane) : urban_elements = {
		environment match {
		case (urban_elements(roads,lanes,crossroads,pedestrian_crossroads,bus_stops,tram_stops,zones)) =>
		return urban_elements(roads,
				lanes :+ lane,
				crossroads, 
				pedestrian_crossroads,
				bus_stops,
				tram_stops,
				zones)
		}
	}

	def addCrossroad(environment : urban_elements, crossroad : crossroad) : urban_elements = {
		environment match {
		case (urban_elements(roads,lanes,crossroads,pedestrian_crossroads,bus_stops,tram_stops,zones)) =>
		return urban_elements(roads,
				lanes,
				crossroads :+ crossroad, 
				pedestrian_crossroads,
				bus_stops,
				tram_stops,
				zones)
		}
	}

	def addPedestrianCrossroad(environment : urban_elements, pedestrian_crossroad : pedestrian_crossroad) : urban_elements = {
		environment match {
		case (urban_elements(roads,lanes,crossroads,pedestrian_crossroads,bus_stops,tram_stops,zones)) =>
		return urban_elements(roads,
				lanes,
				crossroads, 
				pedestrian_crossroads :+ pedestrian_crossroad,
				bus_stops,
				tram_stops,
				zones)
		}
	}

	def addBusStop(environment : urban_elements, bus_stop : bus_stop) : urban_elements = {
		environment match {
		case (urban_elements(roads,lanes,crossroads,pedestrian_crossroads,bus_stops,tram_stops,zones)) =>
		return urban_elements(roads,
				lanes,
				crossroads, 
				pedestrian_crossroads,
				bus_stops :+ bus_stop,
				tram_stops,
				zones)
		}
	}

	def addTramStop(environment : urban_elements, tram_stop : tram_stop) : urban_elements = {
		environment match {
		case (urban_elements(roads,lanes,crossroads,pedestrian_crossroads,bus_stops,tram_stops,zones)) =>
		return urban_elements(roads,
				lanes,
				crossroads, 
				pedestrian_crossroads,
				bus_stops,
				tram_stops :+ tram_stop,
				zones)
		}
	}

	def addZone(environment : urban_elements, zone : zone) : urban_elements = {
		environment match {
		case (urban_elements(roads,lanes,crossroads,pedestrian_crossroads,bus_stops,tram_stops,zones)) =>
		return urban_elements(roads,
				lanes,
				crossroads, 
				pedestrian_crossroads,
				bus_stops,
				tram_stops,
				zones :+ zone)
		}
	}
  
  def removeRoad(environment : urban_elements, road : road) : urban_elements = {
    environment match {
    case (urban_elements(roads,lanes,crossroads,pedestrian_crossroads,bus_stops,tram_stops,zones)) =>
    return urban_elements(roads diff List(road),
        lanes,
        crossroads, 
        pedestrian_crossroads,
        bus_stops,
        tram_stops,
        zones)
    }
  }

  def removeLane(environment : urban_elements, lane : lane) : urban_elements = {
    environment match {
    case (urban_elements(roads,lanes,crossroads,pedestrian_crossroads,bus_stops,tram_stops,zones)) =>
    return urban_elements(roads,
        lanes diff List(lane),
        crossroads, 
        pedestrian_crossroads,
        bus_stops,
        tram_stops,
        zones)
    }
  }

  def removeCrossroad(environment : urban_elements, crossroad : crossroad) : urban_elements = {
    environment match {
    case (urban_elements(roads,lanes,crossroads,pedestrian_crossroads,bus_stops,tram_stops,zones)) =>
    return urban_elements(roads,
        lanes,
        crossroads diff List(crossroad), 
        pedestrian_crossroads,
        bus_stops,
        tram_stops,
        zones)
    }
  }

  def removePedestrianCrossroad(environment : urban_elements, pedestrian_crossroad : pedestrian_crossroad) : urban_elements = {
    environment match {
    case (urban_elements(roads,lanes,crossroads,pedestrian_crossroads,bus_stops,tram_stops,zones)) =>
    return urban_elements(roads,
        lanes,
        crossroads, 
        pedestrian_crossroads diff List(pedestrian_crossroad),
        bus_stops,
        tram_stops,
        zones)
    }
  }

  def removeBusStop(environment : urban_elements, bus_stop : bus_stop) : urban_elements = {
    environment match {
    case (urban_elements(roads,lanes,crossroads,pedestrian_crossroads,bus_stops,tram_stops,zones)) =>
    return urban_elements(roads,
        lanes,
        crossroads, 
        pedestrian_crossroads,
        bus_stops diff List(bus_stop),
        tram_stops,
        zones)
    }
  }

  def removeTramStop(environment : urban_elements, tram_stop : tram_stop) : urban_elements = {
    environment match {
    case (urban_elements(roads,lanes,crossroads,pedestrian_crossroads,bus_stops,tram_stops,zones)) =>
    return urban_elements(roads,
        lanes,
        crossroads, 
        pedestrian_crossroads,
        bus_stops,
        tram_stops diff List(tram_stop),
        zones)
    }
  }

  def removeZone(environment : urban_elements, zone : zone) : urban_elements = {
    environment match {
    case (urban_elements(roads,lanes,crossroads,pedestrian_crossroads,bus_stops,tram_stops,zones)) =>
    return urban_elements(roads,
        lanes,
        crossroads, 
        pedestrian_crossroads,
        bus_stops,
        tram_stops,
        zones diff List(zone))
    }
  }
  
  def merge(maps : List[urban_elements]) : urban_elements = {
    var initial = urban_elements(List[road](), List[lane](), List[crossroad](), List[pedestrian_crossroad](), List[bus_stop](), List[tram_stop](), List[zone]())
    for(map <- maps) {
      map match {
        case urban_elements(roads,lanes,crossroads,pedestrian_crossroads,bus_stops,tram_stops,zones) =>
          for(road <- roads) {
            initial = addRoad(initial,road)
          }
          for(lane <- lanes) {
            initial = addLane(initial,lane)
          }
          for(crossroad <- crossroads) {
            initial = addCrossroad(initial,crossroad)
          }
          for(pedestrian_crossroad <- pedestrian_crossroads) {
            initial = addPedestrianCrossroad(initial,pedestrian_crossroad)
          }
          for(bus_stop <- bus_stops) {
            initial = addBusStop(initial,bus_stop)
          }
          for(tram_stop <- tram_stops) {
            initial = addTramStop(initial,tram_stop)
          }
          for(zone <- zones) {
            initial = addZone(initial,zone)
          }
      }
    }
    return initial
  }

	// metodo per la scrittura finale

	def writeAll(environment : urban_elements, name : String) : Unit = {
		val json = urbanElementsToJValue(environment)
    val finalPath = path.replace("map.json", name)
    val pre_existingFile = new File(finalPath)
    if(pre_existingFile.exists()) {
      println("Removing previous " + name + "...")
      pre_existingFile.delete()
    }
		val output:Output = Resource.fromFile(finalPath)
		output.write(pretty(render(json)))(Codec.UTF8)
	}

}