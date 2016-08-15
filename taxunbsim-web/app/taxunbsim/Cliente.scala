package taxunbsim

import scala.collection.SortedMap

class Cliente(val name: String,
              val saldoCore: Double,
              val saldoBDFIN: Double,
              val saldoPendUNB: SortedMap[NF, Double],
              val nfs: SortedMap[Int, NF]) {

  override def toString(): String = s"""Cliente $name[saldoCore: $saldoCore,
             saldoBDFIN: $saldoBDFIN,
             saldoPendUNB: $saldoPendUNB,
             nfs: $nfs]"""

  def consumoR(cValue: Double, cType: String, c: Cliente): Cliente = {
    cValue match {
      case 0 => c
      case _ => cType match {
                  case "TDATA"   => {
                    val r = (c.nfs.filter(x => x._2.assocValue < x._2.nfValue).head._2).associate(cValue)
                    val usedValue = cValue - r._2
                    val nSaldoPendUNB = c.saldoPendUNB ++ Map(r._1 -> r._1.assocValue)
                    val nc = Cliente(c.name, c.saldoCore - usedValue, c.saldoBDFIN - usedValue, nSaldoPendUNB, c.nfs ++ Map(r._1.nfId -> r._1))
                    consumoR(r._2, cType, nc)
                  }
                  case "TBRASIL" => {
                    val r = (c.nfs.filter(x => x._2.assocValue < x._2.nfValue).head._2).associate(cValue)
                    val usedValue = cValue - r._2
                    val nc = Cliente(c.name, c.saldoCore - usedValue, c.saldoBDFIN - usedValue, c.saldoPendUNB, c.nfs ++ Map(r._1.nfId -> r._1))
                    consumoR(r._2, cType, nc)
                  }
                  case _ => println("Consumo deve ser TDATA ou TBRASIL, devolvendo o mesmo cliente")
                            c
                }
    }
  }

  def consumo(cValue: Double, cType: String): Cliente = {
    if (cValue <= saldoCore) {
      consumoR(cValue, cType, this)
    } else {
      println("Cliente nao tem saldo o suficiente, devolvendo o mesmo cliente")
      this
    }
  }

  def recarga(rechargeValue: Double, rechargeType: String): Cliente = {
    rechargeType match {
      case "PREPG" | "SCRD" => { val r = NF.unburden(rechargeValue,
                                                     saldoPendUNB,
                                                     NF(nfId    = if (nfs.isEmpty) 1 else nfs.lastKey + 1,
                                                        nfType  = rechargeType,
                                                        nfValue = rechargeValue,
                                                        bcValue = rechargeValue,
                                                        svaUNB  = 0.0,
                                                        svaNFs  = List(),
                                                        scrdUNB = 0.0,
                                                        scrdNFs = List(),
                                                        assocValue = 0))
                                  val unbNF = r._2
                                  val nSaldoPendUNB = r._1 ++ (if (rechargeType == "SCRD") SortedMap(unbNF -> unbNF.nfValue) ++ Map() else Map())
                                  Cliente(name, saldoCore + rechargeValue, saldoBDFIN + rechargeValue, nSaldoPendUNB, nfs ++ SortedMap(unbNF.nfId -> unbNF))
                                }
      case _       => { println("Recarga desconhecida, deve ser PREPG ou SCRD, devolvendo o mesmo cliente")
                        this
                      }
    }
  }

}


object Cliente {
  def apply(name: String,
            saldoCore: Double,
            saldoBDFIN: Double,
            saldoPendUNB: SortedMap[NF, Double],
            nfs: SortedMap[Int, NF]) = new Cliente(name, saldoCore, saldoBDFIN, saldoPendUNB, nfs)

}
