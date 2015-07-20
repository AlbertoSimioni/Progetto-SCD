import org.scalatest._

class JSONReaderTest extends UnitSpec {
	
	"Nodes list" should "contain 4 elements" in {
    JSONReader.getAllNodes().length should be (4)
  }
  
  "All nodes" should "have 2 incoming edges" in {
    JSONReader.getAllNodes() foreach { id =>
      JSONReader.getNodeIncomingEdges(id).length should be (2)
    }
  }
  
  "All nodes" should "have 2 outcoming edges" in {
    JSONReader.getAllNodes() foreach { id =>
      JSONReader.getNodeOutcomingEdges(id).length should be (2)
    }
  }
  
  "All nodes" should "have consistent incoming edges" in {
    JSONReader.getAllNodes() foreach { id =>
      JSONReader.getNodeIncomingEdges(id) foreach { edgeId =>
        JSONReader.getAllEdges().contains(edgeId) should be (true)
      }
    }
  }
  
  "All nodes" should "have consistent outcoming edges" in {
    JSONReader.getAllNodes() foreach { id =>
      JSONReader.getNodeOutcomingEdges(id) foreach { edgeId =>
        JSONReader.getAllEdges().contains(edgeId) should be (true)
      }
    }
  }
  
  "Edges list" should "contain 8 elements" in {
    JSONReader.getAllEdges().length should be (8)
  }
  
  "All edges" should "have a consistent begin node" in {
    JSONReader.getAllEdges() foreach { id =>
     JSONReader.getAllNodes().contains(JSONReader.getEdgeStartNode(id)) should be (true) 
    }
  }
  
  "All edges" should "have a consistent end node" in {
    JSONReader.getAllEdges() foreach { id =>
     JSONReader.getAllNodes().contains(JSONReader.getEdgeEndNode(id)) should be (true) 
    }
  }
	
}