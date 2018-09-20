
package urbangrowth.models.coevolution

import java.io.File

import Jama.Matrix

import urbangrowth.models.Model
import urbangrowth.indicators._
import urbangrowth.utils.io.FileUtils
import urbangrowth.utils.math.MatrixUtils

case class Coevolution(
                        /**
                          * Target pop matrix
                          */
                        populationMatrix: Matrix,

                        /**
                          * Distance matrix
                          */
                        distancesMatrix: Matrix,

                        /**
                          * Network feedback distance matrix
                          */
                        feedbackDistancesMatrix: Matrix,

                        /**
                          * Dates
                          */
                        dates: Array[Double],
                        growthRate: Double,
                        gravityWeight: Double,
                        gravityGamma: Double,
                        gravityDecay: Double,
                        feedbackWeight: Double,
                        feedbackGamma: Double,
                        feedbackDecay: Double
                      ) extends Model {

  override def run(): Result = Coevolution.run(model = this)

  override def toString: String = "Coevolution model with parameters"+
   "\n\tgrowthRate = "+growthRate+"\n\tgravityWeight = "+gravityWeight+
  "\n\tgravityGamma = "+gravityGamma+"\n\tgravityDecay = "+gravityDecay+
  "\n\tfeedbackWeight = "+feedbackWeight+"\n\tfeedbackGamma = "+feedbackGamma+
  "\n\tfeedbackDecay = "+feedbackDecay

}



object Coevolution {

  //var populationMatrix: Matrix = null
  //var distancesMatrix: Matrix = null
  //var feedbackDistancesMatrix: Matrix = null
  //var dates: Array[Double] = null
  // setup matrices
  /*def setup(populations: File, distances: File, feedbackDistances: File, datesFile: File) = {
    populationMatrix = FileUtils.parseMatrixFile(populations)
    distancesMatrix = FileUtils.parseMatrixFile(distances)
    feedbackDistancesMatrix = FileUtils.parseMatrixFile(feedbackDistances)
    dates = FileUtils.parseSimple(datesFile)
    //for (t <- 0 to feedbackDistancesMatrix.getColumnDimension() - 1) { print(feedbackDistancesMatrix.get(0, t) + " ; ") }
  }*/

  def apply(populationsFile : File, distancesFile : File, feedbackDistancesFile : File, datesFile: File,
            growthRate: Double,
            gravityWeight: Double,
            gravityGamma: Double,
            gravityDecay: Double,
            feedbackWeight: Double,
            feedbackGamma: Double,
            feedbackDecay: Double
           ) : Coevolution = {

    val populationMatrix: Matrix = FileUtils.parseMatrixFile(populationsFile)
    val distancesMatrix: Matrix = if(distancesFile!=null){
      FileUtils.parseMatrixFile(distancesFile)
    }else{
      new Matrix(populationMatrix.getRowDimension,populationMatrix.getRowDimension,0.0)
    }

    val feedbackDistancesMatrix: Matrix = if(feedbackDistancesFile!=null){
       FileUtils.parseMatrixFile(feedbackDistancesFile)
    }else{
      new Matrix(populationMatrix.getRowDimension,(populationMatrix.getRowDimension*(populationMatrix.getRowDimension-1))/2,0.0)
    }

    val dates = FileUtils.parseSimple(datesFile)
    Coevolution(populationMatrix,distancesMatrix,feedbackDistancesMatrix,dates,
      growthRate,gravityWeight,gravityGamma,gravityDecay,feedbackWeight,feedbackGamma,feedbackDecay
    )
  }


  /**
    *  Run the model
    */
  def run(model: Coevolution): Result = {

    println("Running "+model.toString)

    import model._

    val n = populationMatrix.getRowDimension()
    val p = populationMatrix.getColumnDimension()
    val res = new Matrix(n, p)
    res.setMatrix(0, n - 1, 0, 0, populationMatrix.getMatrix(0, n - 1, 0, 0))

    val gravityDistanceWeights = new Matrix(distancesMatrix.getArray().map { _.map { d => Math.exp(-d / gravityDecay) } })
    val feedbackDistanceWeights = new Matrix(feedbackDistancesMatrix.getArray().map { _.map { d => Math.exp(-d / feedbackDecay) } })

    //println("mean dist mat : " + distancesMatrix.getArray().flatten.sum / (distancesMatrix.getRowDimension() * distancesMatrix.getColumnDimension()))
    //println("mean feedback mat : " + feedbackDistancesMatrix.getArray().flatten.sum / (feedbackDistancesMatrix.getRowDimension() * feedbackDistancesMatrix.getColumnDimension()))

    for (t <- 1 to p - 1) {
      // get time between two dates
      val delta_t = dates(t) - dates(t - 1)

      val prevpop = res.getMatrix(0, n - 1, t - 1, t - 1).copy()
      val totalpop = prevpop.getArray().flatten.sum
      var diagpops = MatrixUtils.diag(prevpop).times(1 / totalpop)
      var diagpopsFeedback = diagpops.times((new Matrix(n, n, 1)).times(diagpops))
      diagpops = new Matrix(diagpops.getArray().map { _.map { Math.pow(_, gravityGamma) } })
      //println("mean norm pop : " + diagpops.getArray().flatten.sum / (n * n))
      diagpopsFeedback = new Matrix(diagpopsFeedback.getArray().map { _.map { Math.pow(_, feedbackGamma) } })
      val potsgravity = diagpops.times(gravityDistanceWeights).times(diagpops)
      val potsfeedback = feedbackDistanceWeights.times(flattenPot(diagpopsFeedback))
      MatrixUtils.setDiag(potsgravity, 0); //setDiag(potsfeedback, 0)
      val meanpotgravity = potsgravity.getArray().flatten.sum / (n * n)
      val meanpotfeedback = potsfeedback.getArray().flatten.sum / n
      //println("mean pot gravity : " + meanpotgravity)
      //println("mean pot feedback : " + meanpotfeedback)
      //val flatpot = flattenPot(potsfeedback)

      res.setMatrix(0, n - 1, t, t,
        prevpop.plus(prevpop.arrayTimes(potsgravity.times(new Matrix(n, 1, 1)).times(gravityWeight / (n * meanpotgravity)).plus(new Matrix(n, 1, growthRate)).plus(
          potsfeedback.times(2 * feedbackWeight / (n * (n - 1) * meanpotfeedback))
        ).times(delta_t)))
      )

    }

    return(Result(populationMatrix,res))
  }

  /**
    *  THIS CODE IS HORRIBLE TO READ - MUST COMMENT IT OR AT LEAST A BIT MORE EXPLICIT
    */
  def flattenPot(m: Matrix): Matrix = {
    val n = m.getRowDimension()
    val res = new Matrix(n * (n - 1) / 2, 1)
    //println(res.getRowDimension)
    for (i <- 0 to n - 2) {
      //println("i :" + i)
      //println("range : " + ((i * (n - 1)) - (i * (i - 1) / 2)) + " ; " + ((i + 1) * (n - 1) - (i * (i + 1) / 2)))
      val col = m.getMatrix(i + 1, n - 1, i, i)
      //println(col.getRowDimension() + " ; " + col.getColumnDimension())
      //println((i + 1) * (n - 1) - (i * (i + 1) / 2) - (i * (n - 1)) - (i * (i - 1) / 2))
      res.setMatrix((i * (n - 1)) - (i * (i - 1) / 2), (i + 1) * (n - 1) - (i * (i + 1) / 2) - 1, 0, 0, col)
    }
    return res
  }



}


