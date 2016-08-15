package taxunbsim

import scala.collection.SortedMap
import scala.math.Ordered.orderingToOrdered

class NF(val nfId: Int,
         val nfType: String,
         val nfValue: Double,
         val bcValue: Double,
         val svaUNB: Double,
         val svaNFs: List[NF],
         val scrdUNB: Double,
         val scrdNFs: List[NF],
         val assocValue: Double) extends Ordered[NF] {

  override def toString(): String = s"""NF $nfId[nfId: $nfId,
                                nfType: $nfType,
                                nfValue: $nfValue,
                                bcValue: $bcValue,
                                svaUNB: $svaUNB,
                                svaNFs: $svaNFs,
                                scrdUNB: $scrdUNB,
                                scrdNFs: $scrdNFs,
                                assocValue: $assocValue]"""

  def compare(that: NF): Int = (this.nfId, this.nfId) compare (that.nfId, that.nfId)

  override def equals(that: Any) = {
    that match {
      case that: NF => this.nfId == that.nfId
      case _ => false
    }

  }

  override def hashCode() = this.nfId

  /**
    * Devolve a NF desonerada e o valor que nao foi possivel desonerar nesta NF
   **/
  def associate(v: Double): (NF, Double) = {
    val max      = this.nfValue - this.assocValue
    //Se o valor a desonerar for maior que o maximo possivel, utilizamos o max
    //Se o maximo possivel comporta todo o valor a desonerar, utilizados todo o valor
    val unbValue = if (v > max) max else v
    val nf = NF(nfId       = this.nfId,
                nfType     = this.nfType,
                nfValue    = this.nfValue,
                bcValue    = this.bcValue,
                svaUNB     = this.svaUNB,
                svaNFs     = this.svaNFs,
                scrdUNB    = this.scrdUNB,
                scrdNFs    = this.scrdNFs,
                assocValue = this.assocValue + unbValue)
    (nf, v - unbValue)
  }

}

object NF {

  def unburden(rechargeValue: Double,
               saldoPendUNB: SortedMap[NF, Double],
               nf: NF): (SortedMap[NF, Double], NF) = {
    rechargeValue match {
      case 0 => (saldoPendUNB, nf)
      case _ => {
                  if (saldoPendUNB.isEmpty || (nf.nfValue - (nf.svaUNB + nf.scrdUNB) == 0)) {
                    (saldoPendUNB, nf)
                  } else {
                    val svaNFs  = saldoPendUNB.filterKeys(k => k.nfType == "PREPG")
                    val scrdNFs = saldoPendUNB.filterKeys(k => k.nfType == "SCRD")
                    if (!svaNFs.isEmpty) {
                      val s = svaNFs.head
                      val r = unburdenNF(s._1, s._2, nf)
                      val nSaldoPendUNB = if (r._1 >= s._2) saldoPendUNB - s._1 else saldoPendUNB ++ Map(s._1 -> r._1)
                      unburden(rechargeValue - r._1, nSaldoPendUNB, r._2)
                    } else {
                      val s = scrdNFs.head
                      val r = unburdenNF(s._1, s._2, nf)
                      val nSaldoPendUNB = if (r._1 >= s._2) saldoPendUNB - s._1 else saldoPendUNB ++ Map(s._1 -> r._1)
                      unburden(rechargeValue - r._1, nSaldoPendUNB, r._2)
                    }
                  }
                }
    }
  }

  def unburdenNF(unbNF: NF, available: Double, nf: NF): (Double, NF) = {
    val maxUnbValue = nf.nfValue - (nf.svaUNB + nf.scrdUNB)
    val unbValue    = if (maxUnbValue < available) available - maxUnbValue else available
    (unbValue, NF(nf.nfId,
                  nf.nfType,
                  nf.nfValue,
                  nf.bcValue - unbValue,
                  if (unbNF.nfType == "PREPG") nf.svaUNB  + unbValue     else nf.svaUNB,
                  if (unbNF.nfType == "PREPG") nf.svaNFs  ++ List(unbNF) else nf.svaNFs,
                  if (unbNF.nfType == "SCRD")  nf.scrdUNB + unbValue     else nf.scrdUNB,
                  if (unbNF.nfType == "SCRD")  nf.scrdNFs ++ List(unbNF) else nf.scrdNFs,
                  0))
  }

  def apply(nfId: Int,
            nfType: String,
            nfValue: Double,
            bcValue: Double,
            svaUNB: Double,
            svaNFs: List[NF],
            scrdUNB: Double,
            scrdNFs: List[NF],
            assocValue: Double) = new NF(nfId, nfType, nfValue, bcValue, svaUNB, svaNFs, scrdUNB, scrdNFs, assocValue)

}
