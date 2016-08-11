import akka.contrib.pattern.ShardCoordinator.ShardAllocationStrategy
import akka.actor.ActorRef
import akka.contrib.pattern.ShardRegion.ShardId
import akka.contrib.pattern.ShardCoordinator.LeastShardAllocationStrategy
import scala.collection.immutable
import scala.concurrent.Future
import scala.math._
import scala.util.control.Breaks._

import map.JSONUtilities._
import map._
import map.Domain._

/*
 * Template per l'implementazione di una strategia di ridistribuzione degli shard
 * 
 * Attualmente implementa la strategia di default
 * Da notare che, in entrambi i metodi
 * - il parametro currentShardAllocations è marcato come immutable
 * - il valore di ritorno è marcato con Future
 * Questo perchè sono strettezze richieste dalla LeastShardAllocationStrategy
 * In un override normale, non sono necessari immutable o future
 */

/*
 * class LeastShardAllocationStrategy(rebalanceThreshold: Int, maxSimultaneousRebalance: Int)
    extends ShardAllocationStrategy with Serializable {

    override def allocateShard(requester: ActorRef, shardId: ShardId,
                               currentShardAllocations: Map[ActorRef, immutable.IndexedSeq[ShardId]]): ActorRef = {
      // identifica semplicemente la regione con meno shard, e la ritorna
      val (regionWithLeastShards, _) = currentShardAllocations.minBy { case (_, v) ⇒ v.size }
      regionWithLeastShards
    }

    override def rebalance(currentShardAllocations: Map[ActorRef, immutable.IndexedSeq[ShardId]],
                           rebalanceInProgress: Set[ShardId]): Set[ShardId] = {
      // controlla che non vi siano shard in movimento più del consentito
      if (rebalanceInProgress.size < maxSimultaneousRebalance) {
        // trova la regione con meno shard
        val (regionWithLeastShards, leastShards) = currentShardAllocations.minBy { case (_, v) ⇒ v.size }
        // trova la regione con più shard (che non siano di quelli in movimento)
        val mostShards = currentShardAllocations.collect {
          case (_, v) ⇒ v.filterNot(s ⇒ rebalanceInProgress(s))
        }.maxBy(_.size)
        // se la differenza tra il set più piccolo e quello più grande è maggiore di una certa soglia
        if (mostShards.size - leastShards.size >= rebalanceThreshold)
          // dichiara il primo shard tra quelli del gruppo maggiore come candidato allo spostamento
          // il risultato di questo metodo verrà dato in pasto ad allocateShard per decidere dove collocarlo
          Set(mostShards.head)
        else
          Set.empty
      } else Set.empty
    }
  }
 */

object ShardingPolicy extends ShardAllocationStrategy with Serializable {
  
  // MOLTO IMPORTANTE
  // Il parametro currentShardAllocations è sempre aggiornato
  // Questo significa che se un nuovo membro è stato definitivamente aggiunto, la sua lista sarà vuota
  // Se un membro è stato definitivamente rimosso, la sua lista è assente (gli shard saranno riallocati non appena vi sarà un messaggio da consegnare per loro)
  
  // Threshold of how large the difference between most and least number of
  // allocated shards must be to begin the rebalancing.
  // rebalance-threshold = 10
  // The number of ongoing rebalancing processes is limited to this number.
  // max-simultaneous-rebalance = 3
  //
  //val defaultStrategy = new LeastShardAllocationStrategy(2, 3)

  override def allocateShard(requester: ActorRef, shardId: ShardId, currentShardAllocations: Map[ActorRef, immutable.IndexedSeq[ShardId]]): ActorRef = {
    /*
    Invoked when the location of a new shard is to be decided
    requester => actor reference to the ShardRegion that requested the location of the shard, can be returned if preference should be given to the node where the shard was first accessed
    shardId => the id of the shard to allocate
    currentShardAllocations =>all actor refs to ShardRegion and their current allocated shards, in the order they were allocated
    returns =>the actor ref of the ShardRegion that is to be responsible for the shard, must be one of the references included in the currentShardAllocations parameter 
    */
    
    // INIZIO CUSTOM ALLOCATESHARD
    // 1) calcola rettangoli, in base al numero di ActorRef nella currentShardAllocations
    // 2) sulla base delle liste di shard, associa liste di shard e rettangoli
    // 3) decreta in quale nodo deve andare lo shard in input
    
    //println("ALLOCATESHARD!")
    //println("currentShardAllocations:")
    val rectangles = computeRectangles(currentShardAllocations.size, current_map_x, current_map_y)
    val map = associateRectanglesNodes(rectangles, currentShardAllocations)
    var target : ActorRef = null
    for(node <- currentShardAllocations) {
      val path = node._1.path.toString
      val rectangle = map(path)
      if(isShardInRectangle(shardId, rectangle) == true) {
        target = node._1
      }
    }
    assert(target != null)
    //println("Lo shard " + shardId + " è stato ora assegnato al nodo " + target.path.toString())
    return target
    // FINE CUSTOM ALLOCATESHARD
    
    /*
    println("ALLOCATESHARD!")
    for(currentNode <- currentShardAllocations) {
      println(currentNode._1.path + " has shards:")
      for(currentShard <- currentNode._2) {
        println(currentShard)
      }
    }
    println("Shard da allocare: " + shardId)
    
    defaultStrategy.allocateShard(requester, shardId, currentShardAllocations)
    */
  }

  override  def rebalance(currentShardAllocations: Map[ActorRef, immutable.IndexedSeq[ShardId]], rebalanceInProgress: Set[ShardId]): Set[ShardId] = {
    /*
    Invoked periodically to decide which shards to rebalance to another location
    currentShardAllocations => all actor refs to ShardRegion and their current allocated shards, in the order they were allocated
    rebalanceInProgress => set of shards that are currently being rebalanced, i.e. you should not include these in the returned set
    returns => the shards to be migrated, may be empty to skip rebalance in this round 
    */
    
    // INIZIO CUSTOM REBALANCE
    // 1) calcola rettangoli, in base al numero di ActorRef nella currentShardAllocations
    // 2) sulla base delle liste di shard, associa liste di shard e rettangoli
    // 3) segnala come "da riallocare" gli shard che non appartengono alle rispettive liste e che non sono già in spostamento
    
    //println("REBALANCE!")
    val rectangles = computeRectangles(currentShardAllocations.size, current_map_x, current_map_y)
    val map = associateRectanglesNodes(rectangles, currentShardAllocations)
    var toBeReallocated = Set[ShardId]()
    for(association <- map) {
      var shardList = List[ShardId]()
      for(node <- currentShardAllocations) {
        if(node._1.path == association._1) {
          shardList = node._2.toList
          break
        }
      }
      val outside = findShardsNotInRectangle(association._2, shardList)
      toBeReallocated = toBeReallocated ++ outside
    }
    val finalSet = toBeReallocated.diff(rebalanceInProgress)
    /*for(shard <- finalSet) {
      println(shard + " è da riallocare")
    }*/
    return finalSet
    
    // FINE CUSTOM REBALANCE
    
    /*
    println("REBALANCE!")
    for(currentNode <- currentShardAllocations) {
      println(currentNode._1.path + " has shards:")
      for(currentShard <- currentNode._2) {
        println(currentShard)
      }
    }
    
    defaultStrategy.rebalance(currentShardAllocations, rebalanceInProgress)
    */
  }
  
  case class point(x : Int, y: Int)
  case class rectangle(bottom_left : point, bottom_right : point, top_left : point, top_right : point)
  
  def computeRectangles(numNodes : Int, map_x : Int, map_y : Int) : List[rectangle] = {
    val factor_x = sqrt(numNodes).ceil.toInt
    var factor_y = factor_x
    while(factor_x * (factor_y - 1) >= numNodes) {
      factor_y = factor_y - 1
    }
    // factor_x * factor_y >= numNodes
    var rectangleList = List[rectangle]()
    val region_x = (map_x.toDouble / factor_x).floor.toInt
    val region_y = (map_y.toDouble / factor_y).floor.toInt
    for(x <- 0 to (factor_x-1)) {
      for(y <- 0 to (factor_y-1)) {
        val bottom_left = point(region_x*x, region_y*y)
        var bottom_right = point(region_x*x + region_x, region_y*y)
        var top_left = point(region_x*x, region_y*y + region_y)
        var top_right = point(region_x*x + region_x, region_y*y + region_y)
        if(x == factor_x - 1) {
          bottom_right = point(map_x, bottom_right.y)
          top_right = point(map_x, top_right.y)
        }
        if(y == factor_y - 1) {
          top_left = point(top_left.x, map_y)
          top_right = point(top_right.x, map_y)
        }
        rectangleList ::= rectangle(bottom_left, bottom_right, top_left, top_right)
      }
    }
    // abbiamo esattamente factor_x * factor_y rettangoli
    // ciascuna delle factor_x colonne ha dunque factor_y rettangoli
    // finchè il numero di rettangoli non è uguale a numNodes, rendi di factor_y - 1 rettangoli la colonna di factor_y rettangoli più a destra
    var rightMost = factor_x-1
    while(rectangleList.length > numNodes) {
      val originalLength = rectangleList.length
      val x = rightMost*region_x
      // rimuovi vecchi
      var toBeRemoved = List[rectangle]()
      for(rectangle <- rectangleList) {
        rectangle.bottom_left.x match {
          case x =>
            toBeRemoved ::= rectangle
        }
      }
      rectangleList = rectangleList.toSet.diff(toBeRemoved.toSet).toList
      // crea nuovi
      val newRegionY = (map_y.toDouble / (factor_y-1)).floor.toInt
      for(y <- 0 to (factor_y-2)) {
        val bottom_left = point(x, newRegionY*y)
        var bottom_right = point(x + region_x, newRegionY*y)
        var top_left = point(x, newRegionY*y + newRegionY)
        var top_right = point(x + region_x, newRegionY*y + newRegionY)
        if(x == (factor_x - 1) * region_x) {
          bottom_right = point(map_x, bottom_right.y)
          top_right = point(map_x, top_right.y)
        }
        if(y == factor_y - 2) {
          top_left = point(top_left.x, map_y)
          top_right = point(top_right.x, map_y)
        }
        rectangleList ::= rectangle(bottom_left, bottom_right, top_left, top_right)
      }
      // diminuisci rightMost
      rightMost = rightMost - 1
      // deve essere vero che...
      assert(originalLength - 1 == rectangleList.length)
    }
    return rectangleList
  }
  
  def associateRectanglesNodes(rectangleList : List[rectangle], currentShardAllocations: Map[ActorRef, immutable.IndexedSeq[ShardId]]) : Map[String, rectangle] = {
    var associationMap = Map[String, rectangle]()
    var modifiableRectangleList = rectangleList
    for(node <- currentShardAllocations) {
      var maxNumberOfShards = 0
      var maxRectangleIndex = -1
      for(rectangle <- modifiableRectangleList) {
        var numberOfShards = 0
        for(shardId <- node._2) {
          if(isShardInRectangle(shardId, rectangle) == true) {
            numberOfShards = numberOfShards + 1
          }
        }
        if((numberOfShards > maxNumberOfShards) || (maxRectangleIndex == -1)) {
          maxNumberOfShards = numberOfShards
          maxRectangleIndex = modifiableRectangleList.indexOf(rectangle)
        }
      }
      // associa nodo a rettangolo
      associationMap = associationMap + (node._1.path.toString -> modifiableRectangleList(maxRectangleIndex))
      // elimina il rettangolo dalla lista
      modifiableRectangleList = modifiableRectangleList.toSet.diff(Set(modifiableRectangleList(maxRectangleIndex))).toList
    }
    return associationMap
  }
  
  def findShardsNotInRectangle(rectangle : rectangle, shardIDs : List[ShardId]) : List[ShardId] = {
    var shardsOutside = List[ShardId]()
    for(shardId <- shardIDs) {
      if(isShardInRectangle(shardId, rectangle) == false) {
        shardsOutside ::= shardId
      }
    }
    return shardsOutside
  }
  
  def isShardInRectangle(shardId : String, rectangle : rectangle) : Boolean = {
    val (_, x, y, _) = splitId(shardId)
    // lati bottom e left inclusi, lati top e right esclusi
    return (x >= rectangle.bottom_left.x &&
            x < rectangle.bottom_right.x &&
            y >= rectangle.bottom_left.y &&
            y < rectangle.top_left.y)
  }
  
}