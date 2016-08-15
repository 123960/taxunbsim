package actors

import akka.actor._
import play.api.libs.json._
import play.api.libs.json.Json.JsValueWrapper
import scala.collection.SortedMap

import taxunbsim._

object WebSocketActor {
  def props(out: ActorRef) = Props(new WebSocketActor(out))
}

class WebSocketActor(out: ActorRef) extends Actor {

  def newClient = Cliente(name = "1", saldoCore = 0, saldoBDFIN = 0, saldoPendUNB = SortedMap[NF, Double](), nfs = SortedMap[Int, NF]())
  var client: Cliente = newClient

  implicit val saldoPendWrites: Writes[(NF, Double)] =
     new Writes[(NF, Double)] {
       def writes(s: (NF, Double)) =
         Json.obj(
           "nf"    -> Json.toJson(s._1),
           "value" -> s._2
         )
  }

  implicit val nfWrites: Writes[NF] = new Writes[NF] {
    def writes(nf: NF) = Json.obj(
      "nfId"       -> nf.nfId,
      "nfType"     -> nf.nfType,
      "nfValue"    -> nf.nfValue,
      "bcValue"    -> nf.bcValue,
      "svaUNB"     -> nf.svaUNB,
      "svaNFs"     -> nf.svaNFs.map(nf => Json.toJson(nf)),
      "scrdUNB"    -> nf.scrdUNB,
      "scrdNFs"    -> nf.scrdNFs.map(nf => Json.toJson(nf)),
      "assocValue" -> nf.assocValue
    )
  }

  implicit val clienteWrites: Writes[Cliente] = new Writes[Cliente] {
    def writes(c: Cliente) = Json.obj(
      "name"        -> c.name,
      "saldoCore"   -> c.saldoCore,
      "saldoBDFIN"  -> c.saldoBDFIN,
      "saldoPendUNB"-> c.saldoPendUNB.map(s => s),
      "nfs"         -> c.nfs.keys.map(k => Json.toJson(c.nfs(k)))
    )
  }

  val rPattern  = "recarga-\\w+-\\d+(\\.*\\,*)\\d*".r
  val rvPattern = "recarga-\\w+-".r
  val cPattern  = "consumo-\\w+-\\d+(\\.*\\,*)\\d*".r
  val tvPattern = "consumo-\\w+-".r

  def receive = {
    case "resetClient" => out ! newClient
    case rPattern(r)   => out ! {client = client.recarga(r.replace("recarga-\\w+-", "").toDouble,
                                                         rvPattern.findFirstIn(r).get.replace("recarga-", "")
                                                         )}
    case cPattern(c)   => out ! {client = client.consumo(c.replace("consumo-\\w+-", "").toDouble,
                                                         tvPattern.findFirstIn(c).get.replace("consumo-", "")
                                                         )}
  }

}
