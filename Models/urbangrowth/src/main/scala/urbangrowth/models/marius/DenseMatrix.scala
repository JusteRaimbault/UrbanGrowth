
package urbangrowth.models.marius


object DenseMatrix {
  def apply(_content: Array[Array[Double]]) = new DenseMatrix {
    override def content: Array[Array[Double]] = _content
  }

  case class Cell(row: Int, value: Double)

}

import DenseMatrix._

trait DenseMatrix {
  def content: Array[Array[Double]]

  def side: Int = content.size
  def lines: Seq[Seq[Cell]] =
    content.map(_.zipWithIndex.map { case (v, i) => Cell(i, v) }.toIndexedSeq).toIndexedSeq

  def transpose = DenseMatrix(content.transpose)
  def linesContent: Seq[Seq[Double]] = content.map(_.toIndexedSeq).toIndexedSeq

  def map(f: (Int, Int, Double) => Double) = {
    val newContent = Array.tabulate(side, side)((i, j) => f(i, j, content(i)(j)))
    DenseMatrix(newContent)
  }

}



