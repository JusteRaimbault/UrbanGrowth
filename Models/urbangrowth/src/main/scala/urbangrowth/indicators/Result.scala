
package urbangrowth.indicators

import Jama.Matrix


case class Result(
                 targetPopulation: Matrix,
                 simulatedPopulation: Matrix,
                 logmse: Double,
                 mselog: Double
                 ) {
  override def toString: String = "logmse = "+logmse+" ; mselog = "+mselog
}




object Result {


  def apply(targetPopulation: Matrix,simulatedPopulation: Matrix): Result = Result(
    targetPopulation,simulatedPopulation,
    Indicators.logmse(simulatedPopulation,targetPopulation),
    Indicators.mselog(simulatedPopulation,targetPopulation)
  )

}
