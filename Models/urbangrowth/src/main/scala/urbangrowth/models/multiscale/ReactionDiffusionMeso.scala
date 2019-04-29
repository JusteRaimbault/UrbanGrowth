package urbangrowth.models.multiscale

import urbangrowth.models.MesoState

import scala.util.Random

import urbangrowth._

object ReactionDiffusionMeso {


  /**
    * iterate a meso step for a set of meso states
    * @param states
    * @param rng
    * @return
    */
  def mesoStep(states: Vector[ReactionDiffusionMesoState])(implicit rng: Random): Vector[ReactionDiffusionMesoState] = {
    states.map{
      s => {
        var state = s
        (0 until s.mesoTimeSteps).foreach{_ =>
          state = state.copy(time=state.time+1,populationGrid=reactionDiffusionStep(s.alpha,s.beta,s.ndiff,s.growthRate)(state.populationGrid))
        }
        state
      }
    }
  }



  /**
    * one step of reaction-diffusion
    * @param alpha
    * @param beta
    * @param diffusionSteps
    * @param growthRate
    * @param config
    * @param rng
    * @return
    */
  def reactionDiffusionStep(alpha: Double,beta: Double,diffusionSteps: Int,growthRate: Double)(config: Vector[Vector[Double]])(implicit rng: Random): Vector[Vector[Double]]={
    val size = config.length
    var arrayVals = config.map{_.toArray}.toArray

    //log("ReacDiff Ng = "+growthRate.toInt)
    //assert(growthRate.toInt>0,"no meso growth : tot pop = "+arrayVals.flatten.sum)
    // growthRates < 1 will lead to no increment : either discretize in smaller units or keep this small discrepency ? (if really pop counts should be reasonable)

    val population = config.flatten.sum
    if (population == 0) {
      //choose random patch
      for (_ <- 1 to growthRate.toInt) { val i = rng.nextInt(size); val j = rng.nextInt(size); arrayVals(i)(j) = arrayVals(i)(j) + 1 }
    } else {
      val oldPop = arrayVals.map(_.map((c: Double) => math.pow(c / population, alpha)))
      val ptot = oldPop.flatten.sum

      for (_ <- 1 to growthRate.toInt) {
        var s = 0.0; val r = rng.nextDouble(); var i = 0; var j = 0
        while (s < r) {
          s = s + (oldPop(i)(j) / ptot)
          j = j + 1
          if (j == size) { j = 0; i = i + 1 }
        }
        if (j == 0) { j = size - 1; i = i - 1 } else { j = j - 1 };
        arrayVals(i)(j) = arrayVals(i)(j) + 1
      }
    }

    // diffuse
    for (_ <- 1 to diffusionSteps) {
      arrayVals = diffuse(arrayVals, beta)
    }

    arrayVals.map(_.toVector).toVector
  }



  /**
    * Diffuse to neighbors proportion beta of capacities
    *
    * @param a
    */
  def diffuse(a: Array[Array[Double]], beta: Double): Array[Array[Double]] = {
    val size = a.size
    val newVals = a.map{_.clone}
    for (i <- a.indices; j <- a(0).indices) {
      // diffuse in neigh cells
      if (i >= 1) { newVals(i - 1)(j) = newVals(i - 1)(j) + (beta / 8) * a(i)(j) }
      if (i < size - 1) { newVals(i + 1)(j) = newVals(i + 1)(j) + (beta / 8) * a(i)(j) }
      if (j >= 1) { newVals(i)(j - 1) = newVals(i)(j - 1) + (beta / 8) * a(i)(j) }
      if (j < size - 1) { newVals(i)(j + 1) = newVals(i)(j + 1) + (beta / 8) * a(i)(j) }
      if (i >= 1 && j >= 1) { newVals(i - 1)(j - 1) = newVals(i - 1)(j - 1) + (beta / 8) * a(i)(j) }
      if (i >= 1 && j < size - 1) { newVals(i - 1)(j + 1) = newVals(i - 1)(j + 1) + (beta / 8) * a(i)(j) }
      if (i < size - 1 && j >= 1) { newVals(i + 1)(j - 1) = newVals(i + 1)(j - 1) + (beta / 8) * a(i)(j) }
      if (i < size - 1 && j < size - 1) { newVals(i + 1)(j + 1) = newVals(i + 1)(j + 1) + (beta / 8) * a(i)(j) }
      newVals(i)(j) = newVals(i)(j) - beta * a(i)(j)
    }
    newVals
  }

}
