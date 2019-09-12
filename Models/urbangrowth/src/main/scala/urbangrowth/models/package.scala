package urbangrowth.models


import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.utils.math.Statistics._
import urbangrowth.indicators.Result


/**
  * Macroscopic state
  *  rq : coordinates are not useful once the distance matrix has been computed ?
  */
trait MacroState {
  def time: Int
  def populations: Vector[Double]
  def distanceMatrix: Vector[Vector[Double]]

  /**
    * Compute macroscopic indicators : population, closeness and accessibility
    * @return
    */
  def indicators: (Vector[Double],Vector[Double],Vector[Double]) = {
    val pop = populations
    val dmat = distanceMatrix
    val ptot = populations.sum
    (pop,dmat.map(r => r.sum/r.size),dmat.map(r=> r.zip(pop).map{case (e,p)=> e*p/ptot}.sum / r.size))
  }

}


/*
object MacroState {
  def initialSyntheticState(): MacroState = MacroState(0)
}
*/


trait MesoState {
  def time: Int
  def populationGrid: Vector[Vector[Double]]

  def morphology: (Double,Double,Double,Double,Double) = {
    val pop = populationGrid.map{_.toArray}.toArray
    val s = slope(pop)
    (GridMorphology.moran(pop),GridMorphology.distanceMean(pop),entropy(pop),s._1,s._2)
  }

}



trait MacroModel {

  //def setup(): Model

  def run(): Result

}



