
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
    val logres = if (m.getColumnDimension==target.getColumnDimension&m.getRowDimension==target.getRowDimension)
      new Matrix(m.getArray().map { _.map { d => Math.log(d) } })
    else new Matrix(target.getRowDimension,target.getColumnDimension,0.0)
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
    //println(m.getColumnDimension)
    //println(m.getRowDimension)
    val sqdiff = if(m.getColumnDimension==target.getColumnDimension&m.getRowDimension==target.getRowDimension)
      m.minus(target).arrayTimes(m.minus(target)) else {
      val zeros = new Matrix(target.getRowDimension,target.getColumnDimension,0.0)
        zeros.minus(target).arrayTimes(zeros.minus(target))
    }
    return Math.log(sqdiff.getArray().flatten.sum)
  }


}
