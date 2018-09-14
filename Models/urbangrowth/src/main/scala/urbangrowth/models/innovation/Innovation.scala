
package urbangrowth.models.innovation

import Jama.Matrix
import java.io.File

import urbangrowth.models.Model
import urbangrowth.utils.io.FileUtils


case class Innovation(
                       /**
                         * Real population matrix
                         */
                       populationMatrix: Matrix,

                       /**
                         * Distance matrix
                         */
                      distanceMatrix: Matrix,

                       /**
                         * Gibrat growth rate
                         */
                       growthRate: Double,

                       /**
                         * weight of innovation induced growth rate
                         */
                       innovationWeight: Double,

                       /**
                         * Decay of gravity interaction
                         */
                       gravityDecay: Double,

                       /**
                         * Decay of innovation diffusion
                         */
                       innovationDecay: Double,

                       /**
                         * Utility of the first innovation
                         */
                        innovationUtility: Double,

                       /**
                         * Growth of the innovation utility (default to 1.12)
                         */
                       innovationUtilityGrowth: Double,

                       /**
                         * Proportion of early adopters (defaults to 1%)
                         */
                       earlyAdoptersRate: Double,

                       /**
                         * City innovation hierarchy : exponent of the probability that a city introduces the new innovation
                         * (defaults to 1)
                         */
                      newInnovationHierarchy: Double,

                       /**
                         * Proportion of population at which new innovation emerges (defaults to 0.5)
                         */
                      newInnovationPopulationProportion: Double

                     ) extends Model {

  override def run(): Matrix = Innovation.run(this)

}


object Innovation {


  def apply(populationFile: File,
            distanceFile: File): Innovation = {
    val populationMatrix = FileUtils.parseMatrixFile(populationFile)
    val distancesMatrix = FileUtils.parseMatrixFile(distanceFile)

  }



  def run(model: Innovation): Matrix = {

  }

}


