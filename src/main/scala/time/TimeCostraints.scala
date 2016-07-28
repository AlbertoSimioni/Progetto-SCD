package time

import com.typesafe.config.ConfigFactory

/**
 * @author Matteo Pozza
 * La classe stabilisce i valori utilizzati nell'avanzamento delle entità mobili
 */
object TimeCostraints {
  
  // unità temporale (in millisecondi)
  val temporalUnit = 5000
  
  // velocità globale dichiarata a configurazione
  val velocityFactor = ConfigFactory.load().getDouble("domain.velocity_factor")
  
  // velocità specifiche
  val pedestrianVelocity = 1.0/5
  val carVelocity = 1.0/50
  val busVelocity = 1.0/50
  val tramVelocity = 1.0/60
  
  // metodo che torna l'amount di millisecondi di intervallo, in base al tipo di entità mobile
  def getVelocityTickInterval(id : String) : Long = {
    id.substring(0, 3) match {
      case "BUS" =>
        return Math.round(temporalUnit * velocityFactor * busVelocity)
      case "TRA" =>
        return Math.round(temporalUnit * velocityFactor * tramVelocity)
      case "PED" =>
        return Math.round(temporalUnit * velocityFactor * pedestrianVelocity)
      case "CAR" =>
        return Math.round(temporalUnit * velocityFactor * carVelocity)
    }
  }
  
}