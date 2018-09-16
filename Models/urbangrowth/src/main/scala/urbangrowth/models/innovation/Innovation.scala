
package urbangrowth.models.innovation

import Jama.Matrix
import java.io.File

import urbangrowth.models.Model
import urbangrowth.utils.io.FileUtils
import urbangrowth.indicators.Result

import scala.collection.mutable.ArrayBuffer

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
                         * Dates
                         */
                       dates: Array[Double],
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

  override def run(): Result = Innovation.run(this)

  override def toString: String = "Innovation model"

}


object Innovation {


  /**
    * Construct from setup files
    *
    * @param populationFile
    * @param distanceFile
    * @param datesFile
    * @param growthRate
    * @param innovationWeight
    * @param gravityDecay
    * @param innovationDecay
    * @param innovationUtility
    * @param innovationUtilityGrowth
    * @param earlyAdoptersRate
    * @param newInnovationHierarchy
    * @param newInnovationPopulationProportion
    * @return
    */
  def apply(populationFile: File,
            distanceFile: File,
            datesFile: File,
            growthRate: Double,
            innovationWeight: Double,
            gravityDecay: Double,
            innovationDecay: Double,
            innovationUtility: Double = 1.0,
            innovationUtilityGrowth: Double = 1.12,
            earlyAdoptersRate: Double = 0.01,
            newInnovationHierarchy: Double = 1.0,
            newInnovationPopulationProportion: Double = 0.5
           ): Innovation = {
    val populationMatrix = FileUtils.parseMatrixFile(populationFile)
    val distancesMatrix = FileUtils.parseMatrixFile(distanceFile)
    val dates = FileUtils.parseSimple(datesFile)

    Innovation(populationMatrix,distancesMatrix,dates,growthRate,innovationWeight,gravityDecay,innovationDecay,innovationUtility,innovationUtilityGrowth,earlyAdoptersRate,newInnovationHierarchy,newInnovationPopulationProportion)
  }



  def run(model: Innovation): Result = {

    println("Running "+model.toString)

    import model._

    val n = populationMatrix.getRowDimension()
    val p = populationMatrix.getColumnDimension()
    val res = new Matrix(n, p)
    res.setMatrix(0, n - 1, 0, 0, populationMatrix.getMatrix(0, n - 1, 0, 0))
    var currentPopulations: Array[Double] = populationMatrix.getMatrix(0, n - 1, 0, 0).getArray.flatten

    val gravityDistanceWeights = new Matrix(distanceMatrix.getArray().map { _.map { d => Math.exp(-d / gravityDecay) } })
    val innovationDistanceWeights = new Matrix(distanceMatrix.getArray().map { _.map { d => Math.exp(-d / innovationDecay) } })

    val innovationUtilities: ArrayBuffer[Double] = new ArrayBuffer[Double]
    innovationUtilities.append(innovationUtility)
    val innovationProportions: ArrayBuffer[Matrix] = new ArrayBuffer[Matrix]
    // the first innovation is already in one city (can be replaced by the second at the first step, consistent as assumed as coming from before the simulated period)

    for (t <- 1 to p - 1) {

    }

    Result(model.populationMatrix,model.populationMatrix)
  }

  def selectCityHierarchically(currentPopulations: Array[Double],alpha: Double): Int = {

  }

}


