
package urbangrowth.models.marius

import DenseMatrix.Cell

/**
  * Bonus component which could be injected in the model
  */
trait Bonus { marius: Marius =>
  /** Factor adjusting bonus value */
  def bonusMultiplier: Double

  /**
    * Compute the bonuses given the diversity and the volume of transactions.
    *
    * @param t the transactions
    * @return a sequence of bonuses indexed by city indexes
    */
  override def bonuses(t: Transacted): Seq[Double] = {
    /** Measure of the exchange partners diversities */
    def diversities = {
      def transactedWith(transacted: Seq[Cell]) =
        transacted.filter { case Cell(_, v) => v > 0 }.map { case Cell(to, _) => to }

      (t.transacted.lines zip t.transposedTransacted.lines) map {
        case (from, to) =>
          (transactedWith(from).toSet union transactedWith(to).toSet).size / t.nbCities.toDouble
      }
    }

    /** Volume of incoming transactions */
    def importVolumes =
      for {
        (demand, i) <- t.demands.zipWithIndex
      } yield t.transactedToSum(i)

    /** Volume of outgoing transactions */
    def exportVolumes =
      for {
        (supply, i) <- t.supplies.zipWithIndex
      } yield t.transactedFromSum(i)

    (importVolumes zip exportVolumes zip diversities) map {
      case ((importVolume, exportVolume), diversity) =>
        bonusMultiplier * (importVolume + exportVolume) * diversity
    }
  }

}


