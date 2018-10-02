
package urbangrowth.utils.io

import java.io.{BufferedReader, File, FileReader}
import Jama.Matrix
import scala.io._


object FileUtils {

  def parseMatrixFile(f: File):Matrix = {
    new Matrix(parseCSV(f, ","))
  }

  def parseCSV(f: File, delimiter: String): Array[Array[Double]] = {
    Source.fromFile(f).getLines.map{_.split(delimiter).map{_.toDouble}.toArray}.toArray

    /*val reader = new BufferedReader(new FileReader(f))
    var currentLine = reader.readLine()
    var res = List((currentLine.split(delimiter).map { s => s.toDouble })) //= new List[Array[Double]] {}
    while (currentLine != null) {
      currentLine = reader.readLine()
      if (currentLine != null) res = res :+ (currentLine.split(delimiter).map { s => s.toDouble }) //+: res
    }
    res.toArray*/
  }

  def parseSimple(f: File) = {
    val reader = new BufferedReader(new FileReader(f))
    var currentLine = reader.readLine()
    var res = List(currentLine.toDouble)
    while (currentLine != null) {
      currentLine = reader.readLine()
      if (currentLine != null) res = res :+ currentLine.toDouble
    }
    res.toArray
  }

}

