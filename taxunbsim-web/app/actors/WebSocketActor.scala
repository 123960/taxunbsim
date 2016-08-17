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

  val rvp = "recarga-\\w+-".r
  val tvp = "consumo-\\w+-".r

  def receive = {
    case "resetClient" => out ! Json.toJson(newClient).toString()
    case op: String    => if (op.contains("recarga")) {
                            client = client.recarga(op.replaceFirst("recarga-\\w+-", "").toDouble,
                                                    rvp.findFirstIn(op).get.replace("recarga-", "")
                                                                           .replace("-", ""))
                            out ! Json.toJson(client).toString()
                          } else if (op.contains("consumo")){
                            client = client.consumo(op.replaceFirst("consumo-\\w+-", "").toDouble,
                                                    tvp.findFirstIn(op).get.replace("consumo-", "")
                                                                           .replace("-", ""))
                            out ! Json.toJson(client).toString()
                          } else {
                            println("none=" + op)
                          }
  }

}
