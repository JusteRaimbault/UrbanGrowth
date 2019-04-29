package urbangrowth.models.multiscale

import Jama.Matrix
import urbangrowth._
import urbangrowth.models.MacroState
import urbangrowth.utils.math.MatrixUtils
import urbangrowth.utils.math.MatrixUtils._

object InteractionMacro {


  def deltaMacroStates(prev: MacroState,current: MacroState): Vector[(Double,Double,Double)] = {
    val prevIndics = prev.indicators
    val currentIndics = current.indicators
    // rq : assumed positive
    val deltaPops = prevIndics._1.zip(currentIndics._1).map{case (p,c) => c - p}
    val deltaPopsmax = deltaPops.max
    val deltaAccs = prevIndics._3.zip(currentIndics._3).map{case (p,c) => c - p}
    val deltaAccsmax = deltaAccs.max
    deltaPops.zip(deltaPops.map(_/deltaPopsmax)).zip(deltaAccs.map(_/deltaAccsmax)).map{case ((dp,dpr),dz) => (dp,dpr,dz)}
  }

  /**
    * macroscopic step
    * @param state
    * @return
    */
  def macroStep(state: InteractionMacroState): InteractionMacroState = {
    val newpops = interactionStep(state.populations,state.distanceMatrix,state.growthRates,state.interactionWeights,state.interactionGammas)
    log("macro Delta P = "+newpops.zip(state.populations).map{case (np,p)=>math.abs(np-p)}.sum)
    state.copy(time = state.time + 1,
      populations = newpops
    )
  }


  /**
    * compute population evolution
    * @param prevpop
    * @param genDistanceMatrix
    * @param growthRates
    * @param interactionWeights
    * @param interactionGammas
    * @return
    */
  def interactionStep(prevpop: Vector[Double],genDistanceMatrix: Vector[Vector[Double]],growthRates: Vector[Double],interactionWeights: Vector[Double],interactionGammas: Vector[Double]): Vector[Double] = {
    val delta_t = 1 // synthetic model
    val n = prevpop.size
    val totalpop = prevpop.toArray.sum
    val diagpops = MatrixUtils.diag(prevpop.toArray.zip(interactionGammas).map{ case (p,g) => math.pow(p / totalpop,g)})
    val dmat = new Matrix(genDistanceMatrix.map(_.toArray).toArray)
    //log("diagpops : "+diagpops.getRowDimension+"x"+diagpops.getColumnDimension+" ; dmat : "+dmat.getRowDimension+"x"+dmat.getColumnDimension)
    val potsgravity = diagpops.times(dmat).times(diagpops)
    MatrixUtils.setDiag(potsgravity, 0)
    val meanpotgravity = potsgravity.getArray().flatten.sum / (prevpop.size * prevpop.size)
    val diagweights = MatrixUtils.diag(interactionWeights.toArray.map(_/ (n * meanpotgravity)))
    val prevpopmat = (new Matrix(Array(prevpop.toArray).transpose))
    prevpopmat.plus(prevpopmat.arrayTimes(diagweights.times(potsgravity).times(new Matrix(n, 1, 1)).plus(new Matrix(Array(growthRates.toArray).transpose)).times(delta_t))).getArray.transpose.head.toVector
  }


  /**
    * compute generalized dist mat from dist mat
    * @param dmat
    * @param decays
    * @return
    */
  def generalizedDistanceMatrix(dmat: Vector[Vector[Double]],decays: Vector[Double]): Vector[Vector[Double]] = {
    val diagDecays = MatrixUtils.diag(decays.toArray.map {1 / _})
    val dm = new Matrix(dmat.map(_.toArray).toArray)
    //log("decays : "+diagDecays.getRowDimension+"x"+diagDecays.getColumnDimension)
    //log("dist mat : "+dm.getRowDimension+"x"+dm.getColumnDimension)
    diagDecays.times(dm).getArray.map(_.toVector.map { d => math.exp(d * (-1.0)) }).toVector
  }

  /**
    * specific update for generalized distance given previous and new decays
    *  ( uses the fact that access is computed with an exponential)
    * @param dmat
    * @param prevDecays
    * @param decays
    * @return
    */
  def updateDistanceMatrix(dmat: Vector[Vector[Double]],prevDecays: Vector[Double],decays: Vector[Double]): Vector[Vector[Double]] =
    repRow(decays.toArray[Double]).getArray.flatten.zip(repRow(prevDecays.toArray).getArray.flatten.toArray[Double]).map{case (d,pd)=> pd/d}.
      zip(new Matrix(dmat.map(_.toArray).toArray).getArray.flatten.toArray[Double]).map{case (e,r)=>math.pow(e,r)}.sliding(decays.size,decays.size).map{_.toVector}.toVector




}
