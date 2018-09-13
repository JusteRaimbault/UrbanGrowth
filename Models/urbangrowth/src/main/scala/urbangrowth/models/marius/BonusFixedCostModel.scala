
package urbangrowth.models.marius


/** Model with fixed costs and bonuses */
class BonusFixedCostModel(
                           val economicMultiplier: Double,
                           val sizeEffectOnSupply: Double,
                           val sizeEffectOnDemand: Double,
                           val distanceDecay: Double,
                           val wealthToPopulationExponent: Double,
                           val populationToWealthExponent: Double,
                           val bonusMultiplier: Double,
                           val fixedCost: Double) extends Marius with Bonus with FixedCost with DefaultValues


object TestModel extends BonusFixedCostModel(
  bonusMultiplier = 197.948890779081,
  fixedCost = 0.256524806806571,
  distanceDecay = 0.672263161474518,
  sizeEffectOnSupply = 1.00175638801509,
  sizeEffectOnDemand = 1.07926078029368,
  economicMultiplier = 0.34380934416368,
  populationToWealthExponent = 1.08660127543912,
  wealthToPopulationExponent = 0.380435604357353
)
