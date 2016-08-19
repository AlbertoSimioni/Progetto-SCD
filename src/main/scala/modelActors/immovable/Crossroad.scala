package modelActors.immovable

import scala.util.Random

import akka.actor.ActorRef

import modelActors.Messages._
import map.Domain.category._

// per gli incroci nil e angle, vogliamo un solo passaggio per corsia => vehicleFreeMap e vehicleFreeTempMap
// per gli incroci di tipo semaphore, un solo passaggio alla volta => vehicleFree e vehicleFreeTemp
// per gli incroci di tipo classic e roundabout, ATTENZIONE! le vehicleFreeMap e vehicleFreeTempMap non sono usate rispetto alle corsie in ingresso, ma rispetto a quelle in uscita
object Crossroad {
  
  // commands
  
  // events
  
  def fromImmovableHandler(myRef : ImmovableActor, myId : String, senderId : String, command : RealCommand) : Unit = {
    command match {
      case FromBusStop(message) =>
        
      case FromCrossroad(message) =>
        
      case FromLane(message) =>
        
      case FromPedestrianCrossroad(message) =>
        
      case FromRoad(message) =>
        
      case FromTramStop(message) =>
        
      case FromZone(message) =>
        
    }
  }
  
  def fromMovableHandler(myRef : ImmovableActor, myId : String, senderId : String, senderRef : ActorRef, command : RealCommand) : Unit = {
    command match {
      case FromPedestrian(message) =>
        
      case FromCar(message) =>
        message match {
          case Vehicle_In(comingFrom, goingTo) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case VehicleBusy(comingFrom, goingTo) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case VehicleFree(comingFrom, goingTo) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
        }
      case FromBus(message) =>
        message match {
          case Vehicle_In(comingFrom, goingTo) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case VehicleBusy(comingFrom, goingTo) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case VehicleFree(comingFrom, goingTo) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
        }
      case FromTram(message) =>
        message match {
          case Vehicle_In(comingFrom, goingTo) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case VehicleBusy(comingFrom, goingTo) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case VehicleFree(comingFrom, goingTo) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
        }
        
    }
  }
  
  def eventHandler(event : Any, state : ImmovableState) : Unit = {
    event match {
      case VehicleBusyArrived(comingFrom) =>
        state.vehicleFree = false
      case VehicleFreeArrived(comingFrom) =>
        state.vehicleFree = true
    }
  }
  
  // UTILITY
  // modella la risposta ad un generico veicolo
  def FromVehicle(myRef : ImmovableActor, myId : String, senderId : String, senderRef : ActorRef, message : Any) : Unit = {
    message match {
      case Vehicle_In(comingFrom, goingTo) =>
        // PRECONDIZIONE: non vi possono essere richieste presenti relative alla stessa lane
        if(myRef.vehicleRequests.contains(comingFrom) == true) {
          println(myRef.state.id + ": SITUAZIONE PROBLEMATICA\nÈ stato ricevuta una richiesta relativa ad una lane che aveva già una richiesta")
          println(myRef.state.id + ": Nuova richiesta: entità " + senderId)
          println(myRef.state.id + ": Richiesta già presente: entità " + myRef.vehicleRequests(comingFrom)._1)
        }
        assert(myRef.vehicleRequests.contains(comingFrom) == false)
        myRef.state.crossroadData.category match {
          case `semaphore` =>
            // a prescindere dall'esito, mettila in vehicle requests
            val tuple = (senderId, senderRef)
            myRef.vehicleRequests = myRef.vehicleRequests + (comingFrom -> tuple)
            if(myRef.state.vehicleFree == true && myRef.vehicleFreeTemp == true) {
              // logica dell'incrocio per decidere chi deve passare
              val toBeSatisfied = semaphoreCrossroadLogic(myRef)
              if(toBeSatisfied._1 != null && toBeSatisfied._2 != null) {
                // poni il corrispondente vehicleFree a false
                myRef.vehicleFreeTemp = false
                myRef.sendToMovable(myId, toBeSatisfied._2, envelope(myId, toBeSatisfied._1, Vehicle_Out))
              }
            }
          case `roundabout` | `classic` =>
            // accoda la richiesta in vehicle requests
            // accoda la richiesta sulla lista relativa alla corsia di destinazione
            // se il lucchetto dell'incrocio è libero
            //   ottieni una lista ordinata delle richieste da soddisfare
            //   finchè una richiesta non è soddisfatta o non è stata raggiunta la fine della lista
            //     controlla se il vehicleFree della corsia di destinazione è libero
            //     se si
            //       imposta il lucchetto a bloccato
            //       imposta il vehiclefree della corsia di destinazione a occupato
            //       rimuovi la richiesta da quelle pendenti relative alla corsia di destinazione
            //       rimuovi la richiesta da vehicle requests
            //       manda vehicleout al veicolo
            val tuple = (senderId, senderRef)
            myRef.vehicleRequests = myRef.vehicleRequests + (comingFrom -> tuple)
            val currentRequestsList = myRef.vehicleDestinationRequests.get(goingTo).getOrElse(List[(String, ActorRef)]())
            if(myRef.vehicleDestinationRequests.contains(goingTo)) {
              myRef.vehicleDestinationRequests = myRef.vehicleDestinationRequests.updated(goingTo, currentRequestsList :+ tuple)
            }
            else {
              myRef.vehicleDestinationRequests = myRef.vehicleDestinationRequests + (goingTo -> (currentRequestsList :+ tuple))
            }
            if(myRef.crossroadLock == true) {
              var requestsList = crossroadLogic(myRef)
              var flag = false
              var index = 0
              while(flag == false && index < requestsList.length) {
                var currentRequest = requestsList(index)
                if(myRef.state.vehicleFreeMap.get(currentRequest._4).getOrElse(true) && myRef.vehicleFreeTempMap.get(currentRequest._4).getOrElse(true)) {
                  myRef.crossroadLock = false
                  if(myRef.vehicleFreeTempMap.contains(currentRequest._4)) {
                    myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap.updated(currentRequest._4, false)
                  }
                  else {
                    myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap + (currentRequest._4 -> false)
                  }
                  val sameLaneRequests = myRef.vehicleDestinationRequests(currentRequest._4)
                  myRef.vehicleDestinationRequests = myRef.vehicleDestinationRequests.updated(currentRequest._4, sameLaneRequests.filter(entry => entry._1 != currentRequest._1))
                  myRef.vehicleRequests = myRef.vehicleRequests - currentRequest._3
                  myRef.sendToMovable(myId, currentRequest._2, envelope(myId, currentRequest._1, Vehicle_Out))
                  flag = true
                }
                else {
                  index = index + 1
                }
              }
            }
          case `nil` | `angle` =>
            // se il tuo vehicle free ti permette di passare, soddisfa immediatamente la richiesta
            if(myRef.state.vehicleFreeMap.get(comingFrom).getOrElse(true) && myRef.vehicleFreeTempMap.get(comingFrom).getOrElse(true)) {
              if(myRef.vehicleFreeTempMap.contains(comingFrom)) {
                myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap.updated(comingFrom, false)
              }
              else {
                myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap + (comingFrom -> false)
              }
              myRef.sendToMovable(myId, senderRef, envelope(myId, senderId, Vehicle_Out))
            }
            else {
              // la richiesta va accodata
              val tuple = (senderId, senderRef)
              myRef.vehicleRequests = myRef.vehicleRequests + (comingFrom -> tuple)
            }
          case _ =>
            println("ERRORE: l'incrocio " + myId + " non è di nessuna delle categorie valide di incrocio")
            assert(false)
        }
      case VehicleBusy(comingFrom, goingTo) =>
        myRef.state.crossroadData.category match {
          case `semaphore` =>
            // per sicurezza, metti a false anche la entry nella tabella temporanea
            myRef.vehicleFreeTemp = false
            // rendi persistente il cambiamento
            // myRef.persistAsync(CrossroadEvent(VehicleBusyArrived(comingFrom))) { evt => }
             // persist body begin
            myRef.state.vehicleFree = false
            // persist body end
          case `roundabout` | `classic` =>
            // metti a false la vehiclefree della mappa permanente
            // sblocca il lucchetto
            // ottieni una lista ordinata delle richieste da soddisfare
            // finchè una richiesta non è soddisfatta o non è stata raggiunta la fine della lista
            //   controlla se il vehicleFree della corsia di destinazione è libero
            //   se si
            //     imposta il lucchetto a bloccato
            //     imposta il vehiclefree della corsia di destinazione a occupato
            //     rimuovi la richiesta da quelle pendenti relative alla corsia di destinazione
            //     rimuovi la richiesta da vehiclerequests
            //     manda vehicleout al veicolo
            if(myRef.vehicleFreeTempMap.contains(goingTo)) {
              myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap.updated(goingTo, false)
            }
            else {
              myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap + (goingTo -> false)
            }
            if(myRef.state.vehicleFreeMap.contains(goingTo)) {
              myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap.updated(goingTo, false)
            }
            else {
              myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap + (goingTo -> false)
            }
            myRef.crossroadLock = true
            var requestsList = crossroadLogic(myRef)
            var flag = false
            var index = 0
            while(flag == false && index < requestsList.length) {
              var currentRequest = requestsList(index)
              if(myRef.state.vehicleFreeMap.get(currentRequest._4).getOrElse(true) && myRef.vehicleFreeTempMap.get(currentRequest._4).getOrElse(true)) {
                myRef.crossroadLock = false
                if(myRef.vehicleFreeTempMap.contains(currentRequest._4)) {
                  myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap.updated(currentRequest._4, false)
                }
                else {
                  myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap + (currentRequest._4 -> false)
                }
                val sameLaneRequests = myRef.vehicleDestinationRequests(currentRequest._4)
                myRef.vehicleDestinationRequests = myRef.vehicleDestinationRequests.updated(currentRequest._4, sameLaneRequests.filter(entry => entry._1 != currentRequest._1))
                myRef.vehicleRequests = myRef.vehicleRequests - currentRequest._3
                myRef.sendToMovable(myId, currentRequest._2, envelope(myId, currentRequest._1, Vehicle_Out))
                flag = true
              }
              else {
                index = index + 1
              }
            }
            
          case `nil` | `angle` =>
            if(myRef.vehicleFreeTempMap.contains(comingFrom)) {
              myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap.updated(comingFrom, false)
            }
            else {
              myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap + (comingFrom -> false)
            }
            if(myRef.state.vehicleFreeMap.contains(comingFrom)) {
              myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap.updated(comingFrom, false)
            }
            else {
              myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap + (comingFrom -> false)
            }
          case _ =>
            println("ERRORE: l'incrocio " + myId + " non è di nessuna delle categorie valide di incrocio")
            assert(false)
        }
      case VehicleFree(comingFrom, goingTo) =>
        myRef.state.crossroadData.category match {
          case `semaphore` =>
            // metti a true la entry nella tabella temporanea
            myRef.vehicleFreeTemp = true
            // rendi persistente il cambiamento
            // myRef.persistAsync(CrossroadEvent(VehicleFreeArrived(comingFrom))) { evt => }
            // persist body begin
            myRef.state.vehicleFree = true
            // persist body end
            // logica dell'incrocio per decidere chi deve passare
            val toBeSatisfied = semaphoreCrossroadLogic(myRef)
            if(toBeSatisfied._1 != null && toBeSatisfied._2 != null) {
              myRef.vehicleFreeTemp = false
              myRef.sendToMovable(myId, toBeSatisfied._2, envelope(myId, toBeSatisfied._1, Vehicle_Out))
            }
          case `roundabout` | `classic` =>
            // imposta il vehicle free della corsia di destinazione a sbloccato
            // se il lucchetto dell'incrocio è libero
            //   ottieni una lista ordinata delle richieste da soddisfare
            //   finchè una richiesta non è soddisfatta o non è stata raggiunta la fine della lista
            //     controlla se il vehicleFree della corsia di destinazione è libero
            //     se si
            //       imposta il lucchetto a bloccato
            //       imposta il vehiclefree della corsia di destinazione a occupato
            //       rimuovi la richiesta da quelle pendenti relative alla corsia di destinazione
            //       rimuovi la richiesta da vehiclerequests
            //       manda vehicleout al veicolo
            if(myRef.vehicleFreeTempMap.contains(goingTo)) {
              myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap.updated(goingTo, true)
            }
            else {
              myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap + (goingTo -> true)
            }
            if(myRef.state.vehicleFreeMap.contains(goingTo)) {
              myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap.updated(goingTo, true)
            }
            else {
              myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap + (goingTo -> true)
            }
            if(myRef.crossroadLock == true) {
              var requestsList = crossroadLogic(myRef)
              var flag = false
              var index = 0
              while(flag == false && index < requestsList.length) {
                var currentRequest = requestsList(index)
                if(myRef.state.vehicleFreeMap.get(currentRequest._4).getOrElse(true) && myRef.vehicleFreeTempMap.get(currentRequest._4).getOrElse(true)) {
                  myRef.crossroadLock = false
                  if(myRef.vehicleFreeTempMap.contains(currentRequest._4)) {
                    myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap.updated(currentRequest._4, false)
                  }
                  else {
                    myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap + (currentRequest._4 -> false)
                  }
                  val sameLaneRequests = myRef.vehicleDestinationRequests(currentRequest._4)
                  myRef.vehicleDestinationRequests = myRef.vehicleDestinationRequests.updated(currentRequest._4, sameLaneRequests.filter(entry => entry._1 != currentRequest._1))
                  myRef.vehicleRequests = myRef.vehicleRequests - currentRequest._3
                  myRef.sendToMovable(myId, currentRequest._2, envelope(myId, currentRequest._1, Vehicle_Out))
                  flag = true
                }
                else {
                  index = index + 1
                }
              }
            }
            
          case `nil` | `angle` =>
            if(myRef.vehicleFreeTempMap.contains(comingFrom)) {
              myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap.updated(comingFrom, true)
            }
            else {
              myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap + (comingFrom -> true)
            }
            if(myRef.state.vehicleFreeMap.contains(comingFrom)) {
              myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap.updated(comingFrom, true)
            }
            else {
              myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap + (comingFrom -> true)
            }
            // guarda se c'è una richiesta pendente relativa alla stessa lane sbloccata
            if(myRef.vehicleRequests.contains(comingFrom)) {
              // rimuovila dalla mappa
              val tuple = myRef.vehicleRequests(comingFrom)
              myRef.vehicleRequests = myRef.vehicleRequests - comingFrom
              if(myRef.vehicleFreeTempMap.contains(comingFrom)) {
                myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap.updated(comingFrom, false)
              }
              else {
                myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap + (comingFrom -> false)
              }
              myRef.sendToMovable(myId, tuple._2, envelope(myId, tuple._1, Vehicle_Out))
            }
          case _ =>
            println("ERRORE: l'incrocio " + myId + " non è di nessuna delle categorie valide di incrocio")
            assert(false)
        }
    }
  }
  
  // UTILITY
  // Regola cosa fare al cambio semaforo
  def HandleSemaphoreSwitch(myRef : ImmovableActor, myId : String) : Unit = {
    if(myRef.state.vehicleFree == true && myRef.vehicleFreeTemp == true) {
      // logica dell'incrocio per decidere chi deve passare
      val toBeSatisfied = semaphoreCrossroadLogic(myRef)
      if(toBeSatisfied._1 != null && toBeSatisfied._2 != null) {
        myRef.vehicleFreeTemp = false
        myRef.sendToMovable(myId, toBeSatisfied._2, envelope(myId, toBeSatisfied._1, Vehicle_Out))
      }
    }
  }
  
  // UTILITY
  // Logica dell'incrocio per decidere se e a chi mandare Vehicle_Out
  // Torna una coppia nulla se nessuno deve ricevere vehicle_out
  // Causa side-effect: qualora venga restituita una coppia, è stata già rimossa dalle vehicleRequests
  // ATTENZIONE: il metodo consulta sempre la tabella delle richieste, quindi non considera l'ipotesi di essere invocato in corrispondenza di una nuova richiesta di passaggio
  // Dunque tutte le richieste vengono messe in vehicle requests, a prescindere se vengono soddisfatte immediatamente o meno
  def semaphoreCrossroadLogic(myRef : ImmovableActor) : (String, ActorRef) = {
    assert(myRef.state.crossroadData.category == `semaphore`)
    // la logica è molto semplice
    // se la corsia che ha il verde ha una richiesta, soddisfala
    // altrimenti, non soddisfare nessuna richiesta
    if(myRef.vehicleRequests.contains(myRef.greenLane)) {
      val tuple = myRef.vehicleRequests.get(myRef.greenLane).get
      myRef.vehicleRequests = myRef.vehicleRequests - myRef.greenLane
      return tuple
    }
    else {
      return (null, null)
    }
    /*
    // prima cosa: capisco che incrocio sono
    myRef.state.crossroadData.category match {
      case `nil` | `angle` =>
        /*
        // tra tutte le richieste pendenti, soddisfane una a caso
        // la maggior parte delle volte ce ne sarà solamente una, e dunque la scelta sarà univoca
        if(myRef.vehicleRequests.size > 0) {
          val toBeSatisfied = Random.shuffle(myRef.vehicleRequests.keys).head
          val tuple = myRef.vehicleRequests.get(toBeSatisfied).get
          myRef.vehicleRequests = myRef.vehicleRequests - toBeSatisfied
          return tuple
        }
        else {
          // nessuna richiesta da soddisfare
          return (null, null)
        }
        */
        println("ERRORE: è stato invocato il metodo di logica dell'incrocio in corrispondenza di un incrocio nil o angle")
        assert(false)
        return (null, null)
      case `classic` =>
        // se abbiamo delle richieste pendenti da una delle corsie senza obbligo di precedenza, soddisfane una
        val precedenceKeys = myRef.crossroadConfiguration.filter(tuple => tuple._2._3.length == 0).keys
        var requests = List[String]()
        for(precedenceKey <- precedenceKeys) {
           if(myRef.vehicleRequests.contains(precedenceKey)) {
             requests = requests :+ precedenceKey
           }
        }
        if(requests.length > 0) {
          val toBeSatisfied = Random.shuffle(requests).head
          val tuple = myRef.vehicleRequests.get(toBeSatisfied).get
          myRef.vehicleRequests = myRef.vehicleRequests - toBeSatisfied
          return tuple
        }
        else {
          // altrimenti, controlla se vi sono delle richieste pendenti sulla corsia(e) che deve aspettare le altre due
          val noPriorityKeys = myRef.crossroadConfiguration.filter(tuple => tuple._2._3.length != 0).keys
          var requests = List[String]()
          for(noPriorityKey <- noPriorityKeys) {
             if(myRef.vehicleRequests.contains(noPriorityKey)) {
               requests = requests :+ noPriorityKey
             }
          }
          if(requests.length > 0) {
            val toBeSatisfied = Random.shuffle(requests).head
            val tuple = myRef.vehicleRequests.get(toBeSatisfied).get
            myRef.vehicleRequests = myRef.vehicleRequests - toBeSatisfied
            return tuple
          }
          else {
            // nessuna richiesta da soddisfare
            return (null, null)
          }
        }
        
      case `semaphore` =>
        // la logica è molto semplice
        // se la corsia che ha il verde ha una richiesta, soddisfala
        // altrimenti, non soddisfare nessuna richiesta
        if(myRef.vehicleRequests.contains(myRef.greenLane)) {
          val tuple = myRef.vehicleRequests.get(myRef.greenLane).get
          myRef.vehicleRequests = myRef.vehicleRequests - myRef.greenLane
          return tuple
        }
        else {
          return (null, null)
        }
        
      case `roundabout` =>
        // prendi in esame tutte le corsie con richiesta, e identifica quelle che hanno la corsia sinistra libera
        var eligibleLanes = List[String]()
        for(lane <- myRef.crossroadConfiguration) {
          if(myRef.vehicleRequests.contains(lane._1) && !myRef.vehicleRequests.contains(lane._2._3(0))) {
            eligibleLanes = eligibleLanes :+ lane._1
          }
        }
        // se la lista di richieste è maggiore di 1, prendine una a caso
        // se la lista è lunga 1, soddisfa la richiesta
        if(eligibleLanes.length > 0) {
          val toBeSatisfied = Random.shuffle(eligibleLanes).head
          val tuple = myRef.vehicleRequests.get(toBeSatisfied).get
          myRef.vehicleRequests = myRef.vehicleRequests - toBeSatisfied
          return tuple
        }
        else {
          // altrimenti, ci rimangono due scenari possibili:
          // ciascuna corsia ha una richiesta sulla sinistra
          // allora scegli una a caso e soddisfala
          // oppure non vi sono richieste
          // allora ritorna null
          if(myRef.vehicleRequests.isEmpty) {
            return (null, null)
          }
          else {
            val toBeSatisfied = Random.shuffle(myRef.vehicleRequests.keys).head
            val tuple = myRef.vehicleRequests.get(toBeSatisfied).get
            myRef.vehicleRequests = myRef.vehicleRequests - toBeSatisfied
            return tuple
          }
        }
    }
    */
  }
  
  // Metodo utilizzato da incroci classic e roundabout
  // Ritorna una lista ordinata di richieste che possono potenzialmente essere soddisfatte
  // È ordinata nel senso che la richiesta soddisfatta dovrebbe essere la prima, e solo nel caso in cui la corsia di destinazione non ha posto a sufficienza si considera la seconda, e così via
  // il metodo non fa side-effect, quindi le richieste non sono rimosse dalla loro mappa
  def crossroadLogic(myRef : ImmovableActor) : List[(String, ActorRef, String, String)] = {
    assert(myRef.state.crossroadData.category == `classic` || myRef.state.crossroadData.category == `roundabout`)
    var result = List[(String, ActorRef, String, String)]()
    if(myRef.state.crossroadData.category == `classic`) {
      // se abbiamo delle richieste pendenti da una delle corsie senza obbligo di precedenza, mettile in lista
      val precedenceKeys = myRef.crossroadConfiguration.filter(tuple => tuple._2._3.length == 0).keys
      var requests = List[String]()
      for(precedenceKey <- precedenceKeys) {
         if(myRef.vehicleRequests.contains(precedenceKey)) {
           requests = requests :+ precedenceKey
         }
      }
      if(requests.length > 0) {
        // dal momento che hanno tutte stessa precedenza, le randomizzo
        val toBeSatisfied = Random.shuffle(requests)
        for(enteringLane <- toBeSatisfied) {
          // trova la corsia di uscita corrispondente
          val tuple = myRef.vehicleRequests(enteringLane)
          assert(myRef.vehicleDestinationRequests.filter(entry => entry._2.contains(tuple)).keys.size == 1)
          val exitingLane = myRef.vehicleDestinationRequests.filter(entry => entry._2.contains(tuple)).keys.head
          // aggiungi la entry a result
          val finalTuple = (tuple._1, tuple._2, enteringLane, exitingLane)
          result = result :+ finalTuple
        }
      }
      // controlla se vi sono delle richieste pendenti sulla corsia(e) che deve aspettare le altre due
      val noPriorityKeys = myRef.crossroadConfiguration.filter(tuple => tuple._2._3.length != 0).keys
      requests = List[String]()
      for(noPriorityKey <- noPriorityKeys) {
         if(myRef.vehicleRequests.contains(noPriorityKey)) {
           requests = requests :+ noPriorityKey
         }
      }
      if(requests.length > 0) {
        val toBeSatisfied = Random.shuffle(requests)
        for(enteringLane <- toBeSatisfied) {
          // trova la corsia di uscita corrispondente
          val tuple = myRef.vehicleRequests(enteringLane)
          assert(myRef.vehicleDestinationRequests.filter(entry => entry._2.contains(tuple)).keys.size == 1)
          val exitingLane = myRef.vehicleDestinationRequests.filter(entry => entry._2.contains(tuple)).keys.head
          // aggiungi la entry a result
          val finalTuple = (tuple._1, tuple._2, enteringLane, exitingLane)
          result = result :+ finalTuple
        }
      }
    }
    else {
      // roundabout
      // prendi in esame tutte le corsie con richiesta, e identifica quelle che hanno la corsia sinistra libera
      var eligibleLanes = List[String]()
      for(lane <- myRef.crossroadConfiguration) {
        if(myRef.vehicleRequests.contains(lane._1) && !myRef.vehicleRequests.contains(lane._2._3(0))) {
          eligibleLanes = eligibleLanes :+ lane._1
        }
      }
      // se la lista di richieste è maggiore di 0, mettile tutte in lista
      if(eligibleLanes.length > 0) {
        for(enteringLane <- eligibleLanes) {
          // trova la corsia di uscita corrispondente
          val tuple = myRef.vehicleRequests(enteringLane)
          assert(myRef.vehicleDestinationRequests.filter(entry => entry._2.contains(tuple)).keys.size == 1)
          val exitingLane = myRef.vehicleDestinationRequests.filter(entry => entry._2.contains(tuple)).keys.head
          // aggiungi la entry a result
          val finalTuple = (tuple._1, tuple._2, enteringLane, exitingLane)
          result = result :+ finalTuple
        }
      }
      else {
        // altrimenti, ci rimangono due scenari possibili:
        // ciascuna corsia ha una richiesta sulla sinistra
        // oppure non vi sono richieste
        if(myRef.vehicleRequests.isEmpty == false) {
          val toBeSatisfied = Random.shuffle(myRef.vehicleRequests.keys)
          for(enteringLane <- toBeSatisfied) {
            // trova la corsia di uscita corrispondente
            val tuple = myRef.vehicleRequests(enteringLane)
            assert(myRef.vehicleDestinationRequests.filter(entry => entry._2.contains(tuple)).keys.size == 1)
            val exitingLane = myRef.vehicleDestinationRequests.filter(entry => entry._2.contains(tuple)).keys.head
            // aggiungi la entry a result
            val finalTuple = (tuple._1, tuple._2, enteringLane, exitingLane)
            result = result :+ finalTuple
          }
        }
      }
    }
    return result
  }
  
}