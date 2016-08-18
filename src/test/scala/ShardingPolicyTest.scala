import org.scalatest._

import main.ShardingPolicy._
import modelActors.immovable.ShardUtilities._

/**
 * @author studio
 */
class ShardingPolicyTest extends UnitSpec {
  
  "The computation of the rectangles" should "behave properly" in {
    val rectangleList = computeRectangles(2, 672, 492)
    for(rectangle <- rectangleList) {
      println("The points of the rectangle are:")
      println("Bottom left: " + rectangle.bottom_left)
      println("Bottom right: " + rectangle.bottom_right)
      println("Top left: " + rectangle.top_left)
      println("Top right: " + rectangle.top_right)
    }
    val dimensions = getDimensionsOfShard(2, 672, 492)
    println("Dimensioni di uno shard: " + dimensions._1 + ", " + dimensions._2)
    rectangleList.size should be (2)
  }
  
  "The shard identification" should "work properly" in {
    val identifier = "C000002400000240"
    val shard = decideShard(identifier)
    shard should be ("S000000000000000")
  }
  
}