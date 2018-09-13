
package urbangrowth.utils.math

import Jama.Matrix


object MatrixUtils {



  def diag(m: Matrix): Matrix = {
    val n = m.getRowDimension()
    val res = Matrix.identity(n, n)
    for (i <- 0 to n - 1) {
      res.set(i, i, m.get(i, 0))
    }
    return res
  }

  def setDiag(m: Matrix, s: Double): Unit = {
    val n = m.getRowDimension()
    for (i <- 0 to n - 1) {
      m.set(i, i, s)
    }
  }

}

