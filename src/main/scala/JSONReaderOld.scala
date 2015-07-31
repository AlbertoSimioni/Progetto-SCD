import org.json4s._
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods._

object JSONReaderOld {
  
  // necessario per corretto parsing
  implicit lazy val formats = DefaultFormats
  
  // case classes di dominio
  case class position(x : Int, y : Int)
  case class node(id : String, position : position, typology : String, incoming_edges : List[String], outcoming_edges : List[String])
  case class edge(id : String, start_node : String, end_node : String)
  case class urbanelements(nodes : List[node], edges : List[edge])
  
  var environment = urbanelements(List[node](), List[edge]())
  
  {
    // blocco di inizializzazione
    val source = scala.io.Source.fromURL(getClass.getResource("/environment.json"))
    val environmentString = try source.getLines mkString finally source.close()
    environment = parse(environmentString, false).extract[urbanelements]
  }
  
  def getAllNodes() : List[String] = {
    var idList = List[String]()
    environment.nodes foreach { node =>
      idList = idList :+ node.id
    }
    return idList
  }
  
  def getAllEdges() : List[String] = {
    var idList = List[String]()
    environment.edges foreach { edge =>
      idList = idList :+ edge.id
    }
    return idList
  }
  
  def getNodePosition(id : String) : (Int, Int) = {
    val node = environment.nodes.find { node => node.id == id }
    if(node.equals(None)) {
      return (0, 0)
    }
    else {
      return (node.get.position.x, node.get.position.y)
    }
  }
  
  def getNodeTypology(id : String) : String = {
    val node = environment.nodes.find { node => node.id == id }
    if(node.equals(None)) {
      return ""
    }
    else {
      return node.get.typology
    }
  }
  
  def getNodeIncomingEdges(id : String) : List[String] = {
    val node = environment.nodes.find { node => node.id == id }
    if(node.equals(None)) {
      return List[String]()
    }
    else {
      return node.get.incoming_edges
    }
  }
  
  def getNodeOutcomingEdges(id : String) : List[String] = {
    val node = environment.nodes.find { node => node.id == id }
    if(node.equals(None)) {
      return List[String]()
    }
    else {
      return node.get.outcoming_edges
    }
  }
  
  def getEdgeStartNode(id : String) : String = {
    val edge = environment.edges.find { edge => edge.id == id }
    if(edge.equals(None)) {
      return ""
    }
    else {
      return edge.get.start_node
    }
  }
  
  def getEdgeEndNode(id : String) : String = {
    val edge = environment.edges.find { edge => edge.id == id }
    if(edge.equals(None)) {
      return ""
    }
    else {
      return edge.get.end_node
    }
  }
  
}