package urbangrowth.models.multiscale

import urbangrowth.models._

case class MultiscaleState(
                          time: Int,
                          macroState: InteractionMacroState,
                          mesoStates: Vector[ReactionDiffusionMesoState]
                          )


/**
  * computed indicators for a multiscale simu
  * @param rawStates
  * @param macroPopulations
  * @param macroClosenesses
  * @param macroAccessibilities
  * @param mesoMorans
  * @param mesoDistances
  * @param mesoEntropy
  * @param mesoSlopes
  * @param mesoSlopeRsquared
  * @param mesoCongestedFlows
  */
case class MultiScaleResult(
                           rawStates: Vector[MultiscaleState],
                           // all flattened indics
                           macroPopulations: Vector[Double],
                           macroClosenesses: Vector[Double],
                           macroAccessibilities: Vector[Double],
                           mesoMorans: Vector[Double],
                           mesoDistances: Vector[Double],
                           mesoEntropy: Vector[Double],
                           mesoSlopes: Vector[Double],
                           mesoSlopeRsquared: Vector[Double],
                           mesoCongestedFlows: Vector[Double],
                           mesoMissingPopulations: Vector[Double]
                           ) {
  def asArrayTuple =  (macroPopulations.toArray,macroClosenesses.toArray,macroAccessibilities.toArray,
    mesoMorans.toArray,mesoDistances.toArray,mesoEntropy.toArray,mesoSlopes.toArray,mesoSlopeRsquared.toArray,
    mesoCongestedFlows.toArray,mesoMissingPopulations.toArray
  )
}

object MultiScaleResult {

  def apply(rawStates: Vector[MultiscaleState]): MultiScaleResult = {
    val morphologies: Vector[(Double,Double,Double,Double,Double)] = rawStates.flatMap{_.mesoStates.map{_.morphology}}
    val macroIndics: Vector[(Vector[Double],Vector[Double],Vector[Double])] = rawStates.map{_.macroState.indicators}
    MultiScaleResult(
      rawStates,
      macroIndics.flatMap(_._1),
      macroIndics.flatMap(_._2),
      macroIndics.flatMap(_._3),
      morphologies.map(_._1),
      morphologies.map(_._2),
      morphologies.map(_._3),
      morphologies.map(_._4),
      morphologies.map(_._5),
      rawStates.flatMap(_.macroState.congestedFlows),
      rawStates.flatMap(_.mesoStates.map(_.missingPopulation))
    )
  }



}

