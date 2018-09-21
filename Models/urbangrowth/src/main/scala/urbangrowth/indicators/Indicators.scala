
package urbangrowth.indicators

import Jama.Matrix


object Indicators {


  /**
    * MSE on logs of populations
    *
    * @param m
    * @param target
    * @return
    */
  def mselog(m: Matrix,target: Matrix): Double = {
    val logres = new Matrix(m.getArray().map { _.map { d => Math.log(d) } })
    val logreal = new Matrix(target.getArray().map { _.map { d => Math.log(d) } })
    val sqdiff = logres.minus(logreal).arrayTimes(logres.minus(logreal))
    return sqdiff.getArray().flatten.sum
  }

  /**
    * Log of MSE
    *
    * @param m
    * @param target
    * @return
    */
  def logmse(m: Matrix,target: Matrix): Double = {
    println(m.getColumnDimension)
    println(m.getRowDimension)
    val sqdiff = m.minus(target).arrayTimes(m.minus(target))
    return Math.log(sqdiff.getArray().flatten.sum)
  }


}
