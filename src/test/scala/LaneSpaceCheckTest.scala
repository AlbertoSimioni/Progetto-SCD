import org.scalatest._

import map.Routes.direction
import map.Domain._
import map.Domain.position._
import modelActors.immovable.Lane._

/**
 * @author studio
 */
class LaneSpaceCheckTest extends UnitSpec {
  
  "A single vehicle going up" should "have access granted and no predecessor nor successor" in {
    var positionsMap = Map[String, point]()
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`right`, true)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (true)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be (null)
    neighbours._2 should be (null)
  }
  
  "A single vehicle going down" should "have access granted and no predecessor nor successor" in {
    var positionsMap = Map[String, point]()
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`left`, false)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (true)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be (null)
    neighbours._2 should be (null)
  }
  
  "A single vehicle going left" should "have access granted and no predecessor nor successor" in {
    var positionsMap = Map[String, point]()
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`up`, false)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (true)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be (null)
    neighbours._2 should be (null)
  }
  
  "A single vehicle going right" should "have access granted and no predecessor nor successor" in {
    var positionsMap = Map[String, point]()
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`down`, true)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (true)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be (null)
    neighbours._2 should be (null)
  }
  
  "A vehicle going up" should "have access granted and a predecessor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(10, 0))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`right`, true)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (true)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be ("CAR0000002")
    neighbours._2 should be (null)
  }
  
  "A vehicle going down" should "have access granted and a predecessor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(10, 20))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`left`, false)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (true)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be ("CAR0000002")
    neighbours._2 should be (null)
  }
  
  "A vehicle going left" should "have access granted and a predecessor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(20, 10))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`up`, false)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (true)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be ("CAR0000002")
    neighbours._2 should be (null)
  }
  
  "A vehicle going right" should "have access granted and a predecessor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(0, 10))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`down`, true)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (true)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be ("CAR0000002")
    neighbours._2 should be (null)
  }
  
  "A vehicle going up" should "have access granted and a successor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(10, 20))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`right`, true)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (true)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be (null)
    neighbours._2 should be ("CAR0000002")
  }
  
  "A vehicle going down" should "have access granted and a successor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(10, 0))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`left`, false)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (true)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be (null)
    neighbours._2 should be ("CAR0000002")
  }
  
  "A vehicle going left" should "have access granted and a successor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(0, 10))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`up`, false)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (true)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be (null)
    neighbours._2 should be ("CAR0000002")
  }
  
  "A vehicle going right" should "have access granted and a successor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(20, 10))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`down`, true)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (true)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be (null)
    neighbours._2 should be ("CAR0000002")
  }
  
  "A vehicle going up" should "have access granted and a predecessor and a successor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(10, 0))
    positionsMap = positionsMap + ("CAR0000003" -> point(10, 20))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`right`, true)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (true)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be ("CAR0000002")
    neighbours._2 should be ("CAR0000003")
  }
  
  "A vehicle going down" should "have access granted and a predecessor and a successor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(10, 20))
    positionsMap = positionsMap + ("CAR0000003" -> point(10, 0))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`left`, false)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (true)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be ("CAR0000002")
    neighbours._2 should be ("CAR0000003")
  }
  
  "A vehicle going left" should "have access granted and a predecessor and a successor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(20, 10))
    positionsMap = positionsMap + ("CAR0000003" -> point(0, 10))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`up`, false)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (true)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be ("CAR0000002")
    neighbours._2 should be ("CAR0000003")
  }
  
  "A vehicle going right" should "have access granted and a predecessor and a successor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(0, 10))
    positionsMap = positionsMap + ("CAR0000003" -> point(20, 10))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`down`, true)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (true)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be ("CAR0000002")
    neighbours._2 should be ("CAR0000003")
  }
  
  "A vehicle going up" should "have no access granted and a predecessor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(10, 9))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`right`, true)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (false)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be ("CAR0000002")
    neighbours._2 should be (null)
  }
  
  "A vehicle going down" should "have no access granted and a predecessor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(10, 11))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`left`, false)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (false)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be ("CAR0000002")
    neighbours._2 should be (null)
  }
  
  "A vehicle going left" should "have no access granted and a predecessor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(11, 10))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`up`, false)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (false)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be ("CAR0000002")
    neighbours._2 should be (null)
  }
  
  "A vehicle going right" should "have no access granted and a predecessor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(9, 10))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`down`, true)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (false)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be ("CAR0000002")
    neighbours._2 should be (null)
  }
  
  "A vehicle going up" should "have no access granted and a successor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(10, 11))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`right`, true)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (false)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be (null)
    neighbours._2 should be ("CAR0000002")
  }
  
  "A vehicle going down" should "have no access granted and a successor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(10, 9))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`left`, false)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (false)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be (null)
    neighbours._2 should be ("CAR0000002")
  }
  
  "A vehicle going left" should "have no access granted and a successor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(9, 10))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`up`, false)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (false)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be (null)
    neighbours._2 should be ("CAR0000002")
  }
  
  "A vehicle going right" should "have no access granted and a successor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(11, 10))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`down`, true)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (false)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be (null)
    neighbours._2 should be ("CAR0000002")
  }
  
  "A vehicle going up" should "have no access granted and a predecessor and a successor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(10, 9))
    positionsMap = positionsMap + ("CAR0000003" -> point(10, 11))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`right`, true)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (false)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be ("CAR0000002")
    neighbours._2 should be ("CAR0000003")
  }
  
  "A vehicle going down" should "have no access granted and a predecessor and a successor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(10, 11))
    positionsMap = positionsMap + ("CAR0000003" -> point(10, 9))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`left`, false)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (false)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be ("CAR0000002")
    neighbours._2 should be ("CAR0000003")
  }
  
  "A vehicle going left" should "have no access granted and a predecessor and a successor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(11, 10))
    positionsMap = positionsMap + ("CAR0000003" -> point(9, 10))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`up`, false)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (false)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be ("CAR0000002")
    neighbours._2 should be ("CAR0000003")
  }
  
  "A vehicle going right" should "have no access granted and a predecessor and a successor" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(9, 10))
    positionsMap = positionsMap + ("CAR0000003" -> point(11, 10))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`down`, true)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (false)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be ("CAR0000002")
    neighbours._2 should be ("CAR0000003")
  }
  
  "A vehicle going up" should "have no access granted and a successor in the same position" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(10, 10))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`right`, true)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (false)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be (null)
    neighbours._2 should be ("CAR0000002")
  }
  
  "A vehicle going down" should "have no access granted and a successor in the same position" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(10, 10))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`left`, false)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (false)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be (null)
    neighbours._2 should be ("CAR0000002")
  }
  
  "A vehicle going left" should "have no access granted and a successor in the same position" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(10, 10))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`up`, false)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (false)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be (null)
    neighbours._2 should be ("CAR0000002")
  }
  
  "A vehicle going right" should "have no access granted and a successor in the same position" in {
    var positionsMap = Map[String, point]()
    positionsMap = positionsMap + ("CAR0000002" -> point(10, 10))
    val id = "CAR0000001"
    val poi = point(10, 10)
    val dir = direction(`down`, true)
    checkEnoughSpace(positionsMap, id, poi, dir) should be (false)
    val neighbours = getNeighbours(positionsMap, poi, dir)
    neighbours._1 should be (null)
    neighbours._2 should be ("CAR0000002")
  }
  
}