package modelActors.immovable

import akka.cluster.Cluster
import scala.math._

import com.typesafe.config.ConfigFactory

import map.Domain._
import map.JSONReader
import map.JSONUtilities._
import main._

object ShardUtilities {

	class ShardNotFoundException extends Exception

	/*
   * Restituisce le dimensioni di uno shard (rettangolo) dati il numero di nodi
   * e le dimensioni della mappa
   */
  def getDimensionsOfShard(numNodes : Int, map_x : Int, map_y : Int) : (Int,Int) = {
    val numShards = 10 * numNodes
    // dobbiamo dividere la mappa in numShards parti il più possibili simili tra loro
    // da ricordare che non ci interessa assolutamente avere esatta precisione: ci va bene che uno shard sia più piccolo di un altro
    // l'unica cosa da mantenere è che il punto più in basso a sinistra dello shard sia dentro la mappa
    // val floatY = sqrt((map_y.toFloat / map_x.toFloat) * numShards).toFloat
    // val floatX = numShards.toFloat / floatY
    val floatX = map_x.toDouble / ceil(sqrt(numShards))
    val floatY = map_y.toDouble / ceil(sqrt(numShards))
    return (ceil(floatX).toInt, ceil(floatY).toInt)
  }
  
  /*
   * Date le dimensioni della mappa e di un singolo shard, restituisce la lista degli id
   * di tutti gli shard della mappa
   */
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
      current_x = current_x + shard_x
    }
    return shardIDs
  }
  
  /*
   * Date le coordinate di un punto e le dimensioni di uno shard,
   * restituisce l'id dello shard di appartenenza 
   */
  def getShardMembership(x : Int, y : Int, shard_x : Int, shard_y : Int) : String = {
    // calcola gli ID degli shard
    val shardIDs = getShardIDs(current_map_x, current_map_y, shard_x, shard_y)
    var index = 0
    while(index < shardIDs.length) {
      val (_, curr_shard_x, curr_shard_y, _) = splitId(shardIDs(index))
      if((x >= curr_shard_x && abs(curr_shard_x - x) <= shard_x) && (y >=curr_shard_y && abs(curr_shard_y - y) <= shard_y)) {
        return shardIDs(index)
      }
      index = index + 1
    }
    // non dovremmo essere qui
    throw new ShardNotFoundException
  }
  
  /*
   * Riceve in input l'id di una entità stradale
   * Restituisce l'id dello shard di appartenenza
   */
  def decideShard(id : String) : String = {
    // recupera il numNodes da configurazione
    //val numNodes = ConfigFactory.load().getInt("domain.num_nodes")
    val numNodes = Cluster(UrbanSimulatorApp.system).state.members.size - 2
    println("proco dio: " +numNodes)
    // calcola le dimensioni del singolo shard
    val (shard_x, shard_y) = getDimensionsOfShard(numNodes, current_map_x, current_map_y)
    // ottieni i dati dall'ID in input
    val (entity, x, y, _) = splitId(id)
    // decidi il comportamento in base al tipo di entità
    var result : String = null
    entity match {
      case "lane" =>
        // deve essere gestita dallo shard di appartenenza dell'entità stradale di destinazione
        val road = JSONReader.getLaneRoad(current_map, id)
        val beginToEnd = JSONReader.getLaneBeginToEnd(current_map, id)
        if(beginToEnd == true) {
          // va preso lo shard di appartenenza della destinazione della strada
          val neighborId = JSONReader.getRoadEndNeighbor(current_map, road)
          val (_, neighbor_x, neighbor_y, _) = splitId(neighborId)
          result = getShardMembership(neighbor_x, neighbor_y, shard_x, shard_y)
        }
        else {
          // va preso lo shard di appartenenza della sorgente della strada
          val neighborId = JSONReader.getRoadBeginNeighbor(current_map, road)
          val (_, neighbor_x, neighbor_y, _) = splitId(neighborId)
          result = getShardMembership(neighbor_x, neighbor_y, shard_x, shard_y)
        }
      /*  
      case "zone" =>
        // trova la lane di appartenenza
        val road = JSONReader.getZoneRoad(current_map, id)
        val position = JSONReader.getZonePosition(current_map, id)
        var lane = ""
        position match {
          case "down" | "left" =>
            val lanes = JSONReader.getRoadLanes(current_map, road).filter(x => x.endsWith("1"))
            lane = lanes.head
          case _ =>
            var lanes = JSONReader.getRoadLanes(current_map, road).filter(x => x.endsWith("3"))
            if(lanes.isEmpty) {
              lanes = JSONReader.getRoadLanes(current_map, road).filter(x => x.endsWith("2"))
            }
            lane = lanes.head
        }
        result = decideShard(lane)*/
      case _ =>
        // restituisci lo shard di appartenenza in base alle coordinate ottenute
        result = getShardMembership(x, y, shard_x, shard_y)
    }
    return result
  }
	
}