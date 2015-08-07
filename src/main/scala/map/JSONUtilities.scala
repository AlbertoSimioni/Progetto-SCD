import com.typesafe.config.ConfigFactory

import Domain._
import scala.math._

object JSONUtilities {
  
  case class OverflowException(x : Int, y : Int) extends Exception
  case class MalformedIdException(id : String) extends Exception
  
  def splitId(id : String) : (String, Int, Int, Int) = {
    checkId(id)
    val entity = id.charAt(0) match {
      case 'R' => "road"
      case 'L' => "lane"
      case 'C' => "crossroad"
      case 'P' => "pedestrian_crossroad"
      case 'B' => "bus_stop"
      case 'T' => "tram_stop"
      case 'Z' => "zone"
      // aggiunta caso speciale: shard
      case 'S' => "shard"
    }
    val x = id.substring(1, 8).toInt
    val y = id.substring(8, 15).toInt
    val z = id.substring(15,16).toInt
    return (entity, x, y, z)
  }
  
  def joinId(kind : String, x : Int, y : Int, z : Int) : String = {
    if(x > 9999999 || y > 9999999) {
      // non è possibile creare un id
      throw OverflowException(x, y)
    }
    val newId = kind.substring(0, 1).toUpperCase() + "%07d".format(x) + "%07d".format(y) + z.toString
    checkId(newId)
    return newId
  }
  
  def checkId(id : String) : Unit = {
    // l'id dovrebbe essere lungo 16 caratteri
    if(id.length() != 16) {
      throw MalformedIdException(id)
    }
    val entity = id.charAt(0)
    val x = id.substring(1, 8).toInt
    val y = id.substring(8, 15).toInt
    val z = id.substring(15,16).toInt
    // l'entità dovrebbe essere una di quelle note
    val possible_entities = List('R','L','C','P','B','T','Z','S')
    val answer = possible_entities.find { a => a == entity }
    if(answer.equals(None)) {
      throw MalformedIdException(id)
    }
    else {
      // se non è una corsia e non termina con 0
      // oppure
      // se è una corsia e termina con 0
      if((entity != 'L' && z != 0) || (entity == 'L' && z == 0)) {
        throw MalformedIdException(id)
      }
      // se i numeri delle coordinate sono molto grandi, potrebbe esserci un errore
      if(x > 1000000 || y > 1000000) {
        println("Attenzione: coordinate molto grandi")
        println("ID: " + id)
      }
    }
  }
  
  def getMapDimensions(map : urban_elements) : (Int,Int) = {
    // il numero di zone in una mappa singola è 37
    val numZones = 37
    var factor = 1
    while((numZones * (factor^2)) != map.zones.length) {
      factor = factor + 1
    }
    return (map_x_dimension * factor, map_y_dimension * factor)
  }
  
  def getDimensionsOfShard(numNodes : Int, map_x : Int, map_y : Int) : (Int,Int) = {
    val numShards = 10 * numNodes
    val floatY = sqrt((map_y.toFloat / map_x.toFloat) * numShards).toFloat
    val floatX = numShards.toFloat / floatY
    return (round(floatX), round(floatY))
  }
  
  def getShardIDs(map_x : Int, map_y : Int, shard_x : Int, shard_y : Int) : List[String] = {
    var shardIDs = List[String]()
    var current_x = 0
    var current_y = 0
    while(current_x < map_x) {
      while(current_y < map_y) {
        // costruisci l'id dello shard
        shardIDs ::= joinId("shard", current_x, current_y, 0)
        // aumenta y
        current_y = current_y + shard_y
      }
      // azzera y
      current_y = 0
      // aumenta x
      current_x = current_x + 1
    }
    return shardIDs
  }
  
  def getShardMembership(x : Int, y : Int, shard_x : Int, shard_y : Int) : String = {
    // calcola gli ID degli shard
    val shardIDs = getShardIDs(current_map_x, current_map_y, shard_x, shard_y)
    var index = 0
    while(index < shardIDs.length) {
      val (_, curr_shard_x, curr_shard_y, _) = splitId(shardIDs(index))
      if(abs(curr_shard_x - x) <= shard_x && abs(curr_shard_y - y) <= shard_y) {
        return shardIDs(index)
      }
      index = index + 1
    }
    // non dovremmo essere qui
    return ""
  }
  
  def decideShard(id : String) : String = {
    // recupera il numNodes da configurazione
    val numNodes = ConfigFactory.load().getInt("domain.num_nodes")
    // calcola le dimensioni del singolo shard
    val (shard_x, shard_y) = getDimensionsOfShard(numNodes, current_map_x, current_map_y)
    // ottieni i dati dall'ID in input
    val (entity, x, y, _) = splitId(id)
    // decidi il comportamento in base al tipo di entità
    entity match {
      case "lane" =>
        // deve essere gestita dallo shard di appartenenza dell'entità stradale di destinazione
        // dunque recupera la mappa
        val map = JSONReader.readAll("map.json")
        val road = JSONReader.getLaneRoad(map, id)
        val beginToEnd = JSONReader.getLaneBeginToEnd(map, id)
        if(beginToEnd == true) {
          // va preso lo shard di appartenenza della destinazione della strada
          val neighborId = JSONReader.getRoadEndNeighbor(map, road)
          val (_, neighbor_x, neighbor_y, _) = splitId(neighborId)
          return getShardMembership(neighbor_x, neighbor_y, shard_x, shard_y)
        }
        else {
          // va preso lo shard di appartenenza della sorgente della strada
          val neighborId = JSONReader.getRoadBeginNeighbor(map, road)
          val (_, neighbor_x, neighbor_y, _) = splitId(neighborId)
          return getShardMembership(neighbor_x, neighbor_y, shard_x, shard_y)
        }
      case "zone" =>
        // trova la lane di appartenenza
        val map = JSONReader.readAll("map.json")
        val road = JSONReader.getZoneRoad(map, id)
        val position = JSONReader.getZonePosition(map, id)
        var lane = ""
        position match {
          case "down" | "left" =>
            val lanes = JSONReader.getRoadLanes(map, road).filter(x => x.endsWith("1"))
            lane = lanes.head
          case _ =>
            var lanes = JSONReader.getRoadLanes(map, road).filter(x => x.endsWith("3"))
            if(lanes.isEmpty) {
              lanes = JSONReader.getRoadLanes(map, road).filter(x => x.endsWith("2"))
            }
            lane = lanes.head
        }
        return decideShard(lane)
      case _ =>
        // restituisci lo shard di appartenenza in base alle coordinate ottenute
        return getShardMembership(x, y, shard_x, shard_y)
    }
  }
  
}