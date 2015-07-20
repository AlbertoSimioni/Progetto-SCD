import akka.contrib.pattern.ShardCoordinator.ShardAllocationStrategy
import akka.actor.ActorRef
import akka.contrib.pattern.ShardRegion.ShardId
import akka.contrib.pattern.ShardCoordinator.LeastShardAllocationStrategy
import scala.collection.immutable
import scala.concurrent.Future

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

object ShardingPolicy extends ShardAllocationStrategy {
  
  // Threshold of how large the difference between most and least number of
  // allocated shards must be to begin the rebalancing.
  // rebalance-threshold = 10
  // The number of ongoing rebalancing processes is limited to this number.
  // max-simultaneous-rebalance = 3
  val defaultStrategy = new LeastShardAllocationStrategy(2, 3)
  
  override def allocateShard(requester: ActorRef, shardId: ShardId, currentShardAllocations: Map[ActorRef, immutable.IndexedSeq[ShardId]]): ActorRef = {
    defaultStrategy.allocateShard(requester, shardId, currentShardAllocations)
  }
  
  override  def rebalance(currentShardAllocations: Map[ActorRef, immutable.IndexedSeq[ShardId]], rebalanceInProgress: Set[ShardId]): Set[ShardId] = {
    defaultStrategy.rebalance(currentShardAllocations, rebalanceInProgress)
  }
  
}