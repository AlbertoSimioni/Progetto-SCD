import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem
import akka.contrib.pattern.ClusterSharding
import akka.actor.Props
import akka.persistence.journal.leveldb.SharedLeveldbJournal
import akka.persistence.journal.leveldb.SharedLeveldbStore
import akka.util.Timeout
import java.util.concurrent.TimeUnit;
import akka.actor.Identify
import akka.pattern.ask
import akka.actor.ActorIdentity
import akka.actor.PoisonPill
import akka.cluster.Cluster
import _root_.akka.actor.ActorSystem
import _root_.akka.io.IO
import _root_.akka.io.Tcp
import _root_.spray.can.Http
import _root_.spray.can.server.UHttp
import com.typesafe.config.ConfigFactory
import api.MainActors
import api.ReactiveApi
import api.ReactiveSecurityConfig
import api.{ MainActors, ReactiveApi, ReactiveSecurityConfig }
import akka.io.{ IO, Tcp }
import java.net.InetSocketAddress
import spray.can.Http
import spray.can.server.UHttp

import pubsub.Subscriber

object UrbanSimulatorApp extends App with ReactiveApi with MainActors  with ReactiveSecurityConfig {

  // la porta viene letta da file di configurazione
  // l'override della porta non funziona a causa della keyword "lazy"
  // pertanto è necessario scrivere la porta in file di configurazione PRIMA dell'accensione del sistema
  // la riga sottostante non è necessaria
  
  // crea la configurazione
  //val config = ConfigFactory.parseString("akka.remote.netty.tcp.port = " + args(0))
  //                  .withFallback(ConfigFactory.parseString(""" akka.cluster.roles = ["""" + args(1) + """"]"""))
  //                  .withFallback(ConfigFactory.load())
	
  // crea l'ActorSystem
	implicit lazy val system = ActorSystem("UrbanSimulator", ConfigFactory.load())
  
  // inizializza il DB
	// un DB comune in un solo nodo, il seed
	//startUpDB(system, port)
	/*
	 * @param typeName the name of the entry type
	 * @param entryProps the `Props` of the entry actors that will be created by the `ShardRegion`,
	 *   if not defined (None) the `ShardRegion` on this node will run in proxy only mode, i.e.
	 *   it will delegate messages to other `ShardRegion` actors on other nodes, but not host any
	 *   entry actors itself
	 * @param idExtractor partial function to extract the entry id and the message to send to the
	 *   entry from the incoming message, if the partial function does not match the message will
	 *   be `unhandled`, i.e. posted as `Unhandled` messages on the event stream
	 * @param shardResolver function to determine the shard id for an incoming message, only messages
	 *   that passed the `idExtractor` will be used
	 * @param allocationStrategy possibility to use a custom shard allocation and
	 *   rebalancing logic
	 * @return the actor ref of the [[ShardRegion]] that is to be responsible for the shard
	 */
	val shardRegionActor = ClusterSharding(system).start(
	  typeName = UrbanElement.typeOfEntries,
		entryProps = Some(UrbanElement.props()),
		idExtractor = UrbanElement.idExtractor,
		shardResolver = UrbanElement.shardResolver,
		allocationStrategy = ShardingPolicy)
  
	Cluster(system) registerOnMemberUp {
		// recupero il ruolo
		val role = system.settings.config.getList("akka.cluster.roles").get(0).unwrapped
		// avvio seed node + controller
		if(role == "controller") {
			// attiva Controller
			val controller = system.actorOf(Props[Controller])
			controller ! StartInjection
		}
		// avvio server http
    else if(role == "guihandler") {
			IO(UHttp) ! Http.Bind(wsService, Configuration.host, Configuration.portWs)
			// Since the UTttp extension extends from Http extension, it starts an actor whose name will later collide with the Http extension.
			system.actorSelection("/user/IO-HTTP") ! PoisonPill
			// We could use IO(UHttp) here instead of killing the "/user/IO-HTTP" actor
			IO(Http) ! Http.Bind(rootService, Configuration.host, Configuration.portHttp)
			sys.addShutdownHook({ IO(UHttp) ! Http.Unbind; IO(Http) ! Http.Unbind; system.shutdown })
      system.actorOf(Props[Subscriber], "subscriber1")
		}
	}
}

// configurazione per Server HTTP
object Configuration {
  import com.typesafe.config.ConfigFactory

  private val config = ConfigFactory.load
  config.checkValid(ConfigFactory.defaultReference)

  lazy val host = config.getString("mysample.host")
  lazy val portHttp = config.getInt("mysample.ports.http")
  lazy val portWs = config.getInt("mysample.ports.ws")
}

/*
 def startUpDB(system : ActorSystem, port : String) : Unit = {
    if(port == "2551") {
      system.actorOf(Props[SharedLeveldbStore], "store")
    }
    // recupera il riferimento al seed node
    // executioncontext.implicit.global importato perchè parametro "occulto" di onSuccess e onFailure
    // timeout dichiarato implicito perchè parametro "occulto" di ask (messaggio sincrono ?)
    import scala.concurrent.ExecutionContext.Implicits.global
    implicit val timeout = Timeout(10,TimeUnit.SECONDS)
    val path = "akka.tcp://UrbanSimulator@127.0.0.1:2551/user/store"
    val answer = (system.actorSelection(path) ? Identify(None))
    answer.onSuccess {
      case ActorIdentity(_, Some(ref)) => SharedLeveldbJournal.setStore(ref, system)
      case _ =>
        system.log.error("Shared journal not started at {}", path)
        system.terminate()
    }
    answer.onFailure {
      case _ =>
        system.log.error("Lookup of shared journal at {} timed out", path)
        system.terminate()
    }
  }
*/