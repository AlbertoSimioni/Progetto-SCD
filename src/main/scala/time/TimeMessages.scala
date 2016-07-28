package time

import scala.util.Random._

/**
 * @author Matteo Pozza
 * La classe modella tutti i case object e case classes usati nella gestione di eventi temporali
 */
object TimeMessages {
  
  val timeMessage = "TimeMessage"
  
  // mandato dal gestore del tempo a se stesso
  case object Tick
  
  // oggetto che rappresenta un orario
  case class TimeValue(hours : Int, minutes : Int)
  
  // comando ed evento per il rilascio di un orario
  case class TimeCommand(timeValue : TimeValue)
  case class TimeEvent(timeValue : TimeValue)
  
  def sameTime(referenceValue : TimeValue, comparedValue : TimeValue) : Boolean = {
    return ((referenceValue.hours == comparedValue.hours) && (referenceValue.minutes == comparedValue.minutes))
  }
  
  def isLate(referenceValue : TimeValue, comparedValue : TimeValue) : Boolean = {
    // se l'orario da confrontare è avanti fino a 12 ore (orario stesso incluso, 12 ore escluse), siamo in ritardo
    // se l'orario da confrontare è indietro fino a 12 ore (orario stesso escluso, 12 ore incluse), siamo in anticipo
    if(referenceValue.hours < comparedValue.hours) {
      if(comparedValue.hours - referenceValue.hours > 12) {
        // sicuramente siamo in anticipo
        return false
      }
      else if(comparedValue.hours - referenceValue.hours < 12) {
        // sicuramente siamo in ritardo
        return true
      }
      else {
        if(comparedValue.minutes >= referenceValue.minutes) {
          // sicuramente siamo in anticipo
          return false
        }
        else {
          // siamo sicuramente in ritardo
          return true
        }
      }
    }
    else if(referenceValue.hours > comparedValue.hours) {
      if(referenceValue.hours - comparedValue.hours > 12) {
        // sicuramente siamo in ritardo
        return true
      }
      else if(referenceValue.hours - comparedValue.hours < 12) {
        // sicuramente siamo in anticipo
        return false
      }
      else {
        if(referenceValue.minutes > comparedValue.minutes) {
          // sicuramente siamo in ritardo
          return true
        }
        else {
          // siamo sicuramente in anticipo
          return false
        }
      }
    }
    else {
      if(referenceValue.minutes > comparedValue.minutes) {
        // sicuramente siamo in anticipo
        return false
      }
      else {
        // siamo sicuramente in ritardo
        return true
      }
    }
  }
  
  /*
   * Genera tre orari casualmente, separati da otto ore ciascuno
   */
  def createTimes() : (TimeValue, TimeValue, TimeValue) = {
    // 1440 minuti in 24 ore
    // 480 minuti in 8 ore
    val houseEndTime = nextInt(1440)
    val workEndTime = (houseEndTime + 480) % 1440
    val funEndTime = (workEndTime + 480) % 1440
    return (minutesToTime(houseEndTime), minutesToTime(workEndTime), minutesToTime(funEndTime))
  }
  
  /*
   * Funzione di utilità: converte il numero di minuti totali in una coppia ore-minuti
   */
  def minutesToTime(minutes : Int) : TimeValue = {
    return TimeValue(minutes / 60, minutes % 60)
  }
  
}