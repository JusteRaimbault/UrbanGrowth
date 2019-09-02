package urbangrowth.models.multiscale

import org.openmole.spatialdata._
import org.openmole.spatialdata.grid.synthetic.ExpMixtureGenerator
import urbangrowth.models.MesoState

import scala.util.Random

case class ReactionDiffusionMesoState(
                                     override val time: Int,
                                     override val populationGrid: Vector[Vector[Double]],
                                     alpha: Double,
                                     beta: Double,
                                     ndiff: Int,
                                     growthRate: Double,
                                     mesoTimeSteps: Int,
                                     missingPopulation: Double
                                     ) extends MesoState {

}


object ReactionDiffusionMesoState {

  /**
    * monocentric initial metropolitan config
    * @param gridSize
    * @return
    */
  def initialSyntheticState(gridSize: Int,centerDensity: Double,kernelRadius: Double,
                            alpha: Double,beta: Double,ndiff: Int,growthRate: Double,
                            mesoTimeSteps: Int)(implicit rng: Random): ReactionDiffusionMesoState = ReactionDiffusionMesoState(
    0,
    ExpMixtureGenerator(gridSize,1,centerDensity,kernelRadius,false,Seq((gridSize/2,gridSize/2))).generateGrid.map{_.toVector}.toVector,
    alpha,beta,ndiff,growthRate,mesoTimeSteps,0.0
  )

}
