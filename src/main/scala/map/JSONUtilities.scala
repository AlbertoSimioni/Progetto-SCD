package map;

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
  
}