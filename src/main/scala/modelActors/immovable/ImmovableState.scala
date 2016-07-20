package modelActors.immovable

import akka.persistence.AtLeastOnceDelivery.AtLeastOnceDeliverySnapshot

import map.Domain._

/**
 * @author Matteo Pozza
 * Questa classe modella lo stato di un TestActor
 * Deve essere comprensiva di tutti i dati che potrebbero servire ad un qualunque "sottotipo"
 * Ciascun attore utilizzerà poi solo i dati a lui necessari in base al ruolo
 */
class ImmovableState {
  
  // lista degli identificativi delle entità mobili sotto la propria gestione
  var handledMobileEntities = List[String]()
  
  // AT-LEAST-ONCE
  // Stato della at-least-once dell'attore
  //var deliveryState : AtLeastOnceDeliverySnapshot = null
  
  // AT-LEAST-ONCE
  // filtro dei duplicati
  var lastMessages = Map[String, Long]()
  
  // AT-LEAST-ONCE
  // test sul filtro dei duplicati
  def isNewMessage(actorId : String, deliveryId : Long) : Boolean = {
    if(lastMessages.contains(actorId)) {
      if(lastMessages.get(actorId).get >= deliveryId) {
        return false
      }
      else {
        return true
      }
    }
    else {
      return true
    }
  }
  
  // AT-LEAST-ONCE
  // aggiornamento del filtro dei duplicati: se il messaggio è nuovo, aggiorna la mappa
  def updateFilter(actorId : String, deliveryId : Long) : Unit = {
    if(lastMessages.contains(actorId)) {
      lastMessages = lastMessages.updated(actorId, deliveryId)
    }
    else {
      lastMessages = lastMessages + (actorId -> deliveryId)
    }
  }
  
  
  
  var id : String = null
  
  var bus_stopData : bus_stop = null
  var crossroadData : crossroad = null
  var laneData : lane = null
  var pedestrian_crossroadData : pedestrian_crossroad = null
  var roadData : road = null
  var tram_stopData : tram_stop = null
  var zoneData : zone = null
  
}