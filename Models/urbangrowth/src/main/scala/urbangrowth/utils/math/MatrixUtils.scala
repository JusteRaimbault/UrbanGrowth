
package urbangrowth.utils.math

import Jama.Matrix


object MatrixUtils {


  def rowMatrix(d:Array[Double]): Matrix = {
    new Matrix(d,1)
  }

  def colMatrix(d:Array[Double]): Matrix = {
    new Matrix(d,d.length)
  }



  def diag(m: Matrix): Matrix = {
    val n = m.getRowDimension()
    val res = Matrix.identity(n, n)
    for (i <- 0 to n - 1) {
      res.set(i, i, m.get(i, 0))
    }
    return res
  }

  def diag(d : Array[Double]): Matrix = {
    val res = new Matrix(d.length,d.length,0.0)
    for(i <- 0 to d.length - 1){res.set(i,i,d(i))}
    res
  }

  def setDiag(m: Matrix, s: Double): Unit = {
    val n = m.getRowDimension()
    for (i <- 0 to n - 1) {
      m.set(i, i, s)
    }
  }

}

