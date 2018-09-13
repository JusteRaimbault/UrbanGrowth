
package urbangrowth.models.marius



trait FixedCost <: Marius {

  /** Cost of a transaction */
  def fixedCost: Double

  /** Cost implied by the number of transactions */
  override def totalFixedCosts(t: Transacted): Seq[Double] =
    t.transacted.linesContent.map { _.count(_ > 0.0) * fixedCost }

  /** Filter the interaction potential matrix */
  override def interactionPotentialMatrix(supplies: Seq[Double], demands: Seq[Double], network: FullNetwork): DenseMatrix = {
    val interactionMatrixValue = super.interactionPotentialMatrix(supplies, demands, network)
    val fromInteractionPotentialSum = interactionMatrixValue.transpose.linesContent.map(_.sum)

    interactionMatrixValue.map {
      (from, to, ip) =>
        if (ip > 0) {
          val fSupply = supplies(from)
          val fromIPSum = fromInteractionPotentialSum(from)
          val normalisedIPFrom = ip / fromIPSum
          if (normalisedIPFrom * fSupply > fixedCost) ip else 0.0
        } else 0.0
    }
  }

}
