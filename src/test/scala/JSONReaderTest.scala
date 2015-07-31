import org.scalatest._

class JSONReaderTest extends UnitSpec {
	
	"Nodes list" should "contain 4 elements" in {
    JSONReaderOld.getAllNodes().length should be (4)
  }
  
  "All nodes" should "have 2 incoming edges" in {
    JSONReaderOld.getAllNodes() foreach { id =>
      JSONReaderOld.getNodeIncomingEdges(id).length should be (2)
    }
  }
  
  "All nodes" should "have 2 outcoming edges" in {
    JSONReaderOld.getAllNodes() foreach { id =>
      JSONReaderOld.getNodeOutcomingEdges(id).length should be (2)
    }
  }
  
  "All nodes" should "have consistent incoming edges" in {
    JSONReaderOld.getAllNodes() foreach { id =>
      JSONReaderOld.getNodeIncomingEdges(id) foreach { edgeId =>
        JSONReaderOld.getAllEdges().contains(edgeId) should be (true)
      }
    }
  }
  
  "All nodes" should "have consistent outcoming edges" in {
    JSONReaderOld.getAllNodes() foreach { id =>
      JSONReaderOld.getNodeOutcomingEdges(id) foreach { edgeId =>
        JSONReaderOld.getAllEdges().contains(edgeId) should be (true)
      }
    }
  }
  
  "Edges list" should "contain 8 elements" in {
    JSONReaderOld.getAllEdges().length should be (8)
  }
  
  "All edges" should "have a consistent begin node" in {
    JSONReaderOld.getAllEdges() foreach { id =>
     JSONReaderOld.getAllNodes().contains(JSONReaderOld.getEdgeStartNode(id)) should be (true) 
    }
  }
  
  "All edges" should "have a consistent end node" in {
    JSONReaderOld.getAllEdges() foreach { id =>
     JSONReaderOld.getAllNodes().contains(JSONReaderOld.getEdgeEndNode(id)) should be (true) 
    }
  }
	
}