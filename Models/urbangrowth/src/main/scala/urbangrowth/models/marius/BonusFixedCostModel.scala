
package urbangrowth.models.marius

import java.io.File


/** Model with fixed costs and bonuses */
case class BonusFixedCostModel(
                           modelDistanceMatrix: DistanceMatrix,
                           modelConfiguration: MariusConfiguration,
                           economicMultiplier: Double,
                           sizeEffectOnSupply: Double,
                           sizeEffectOnDemand: Double,
                           distanceDecay: Double,
                           wealthToPopulationExponent: Double,
                           populationToWealthExponent: Double,
                           bonusMultiplier: Double,
                           fixedCost: Double) extends Marius with Bonus with FixedCost //with DefaultValues
{
  override def distanceMatrix: DistanceMatrix = modelDistanceMatrix

  override def configuration: MariusConfiguration = modelConfiguration

  override def maxStep: Int = modelConfiguration.dates.length - 1
}

object BonusFixedCostModel {
  def apply(distanceMatrixFile: File,
            confFile: File,
            datesFile: File,
            economicMultiplier: Double,
            sizeEffectOnSupply: Double,
            sizeEffectOnDemand: Double,
            distanceDecay: Double,
            wealthToPopulationExponent: Double,
            populationToWealthExponent: Double,
            bonusMultiplier: Double,
            fixedCost: Double): BonusFixedCostModel =
    BonusFixedCostModel(MariusFile.distances(distanceMatrixFile),MariusFile.configFromFile(confFile,datesFile) ,economicMultiplier,sizeEffectOnSupply,
      sizeEffectOnDemand, distanceDecay, wealthToPopulationExponent, populationToWealthExponent,bonusMultiplier,fixedCost)
}




