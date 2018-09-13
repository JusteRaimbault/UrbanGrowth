
package urbangrowth.models.marius


/** Simple model with only core mechanisms */
class SimpleModel(
                   val economicMultiplier: Double,
                   val sizeEffectOnSupply: Double,
                   val sizeEffectOnDemand: Double,
                   val distanceDecay: Double,
                   val wealthToPopulationExponent: Double,
                   val populationToWealthExponent: Double) extends Marius with DefaultValues
