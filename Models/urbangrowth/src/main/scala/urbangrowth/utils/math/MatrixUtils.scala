
package urbangrowth.utils.math

import Jama.Matrix

import urbangrowth._

import scala.util.Random


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

  def repRow(d: Array[Double]): Matrix = diag(d).times(new Matrix(d.size,d.size,1.0))

  def repCol(d: Array[Double]): Matrix = (new Matrix(d.size,d.size,1.0)).times(diag(d))


  // implicit conversions to and from matrices ?
  //class MatrixDecorator


  /**
    * random distance matrix
    * @param n
    * @param worldSize
    * @param rng
    * @return
    */
  def randomDistanceMatrix(n: Int,worldSize: Double)(implicit rng: Random): Vector[Vector[Double]] = {
    val xcoords = Vector.fill(n)(rng.nextDouble()*worldSize)
    val ycoords = Vector.fill(n)(rng.nextDouble()*worldSize)

    val flatres = for {
      x1 <- xcoords.zip(ycoords)
      x2 <- xcoords.zip(ycoords)
    } yield math.sqrt(math.pow((x1._1-x2._1),2)+math.pow(x1._2-x2._2,2))

    log("dmat flat : "+flatres.size)

    //flatres.sliding(n).toVector
    flatres.sliding(n,n).toVector
  }






}

