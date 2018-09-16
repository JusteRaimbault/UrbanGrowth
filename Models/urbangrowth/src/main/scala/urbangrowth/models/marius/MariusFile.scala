
package urbangrowth.models.marius

import scala.io.Source
import java.io.File

import Jama.Matrix


object MariusFile {

  /** Read the content of the file */
  def content(confFile: File):Iterator[Seq[String]] = {
    val input =
      //Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("fr/geocites/marius/marius.csv"))
      Source.fromFile(confFile)

    input.getLines.map {
      l => l.split(",").toSeq
    }
  }

  def getdates(datesFile: File): Seq[Int] = {
    val input =Source.fromFile(datesFile)
    input.getLines.map{_.toInt}.toSeq
  }

  def distances(distFile: File) : DistanceMatrix = {
    val input =
      //Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("fr/geocites/marius/distances.csv"))
      Source.fromFile(distFile)

    input.getLines.map {
      l => l.split(",").map{_.toDouble}.toSeq
    }.toSeq
  }

  /**
    * Column of population at a given date
    *
    * @param date date of observation
    * @return an option containing the population if provided, none otherwise
    */
  def populations(date: Int,columnsBeforeDates: Int,startingCities: List[Seq[String]],dates: Seq[Int]): Option[Seq[Double]] =
    (dates.indexOf(date) match {
      case -1 => None
      case i => Some(i + columnsBeforeDates)
    }).map {
      c => startingCities.map(_(c).toDouble)
    }

  def configFromFile(confFile : File,datesFile: File): MariusConfiguration = {
    val csv = content(confFile)
    val header: Seq[String] = csv.next
    val data = csv.drop(1).toList

    // check if marius initial file or without specific info
    val isMariusFile: Boolean = header.contains("EastWest")


    //val dates: Seq[Int] = if(isMariusFile) header.takeRight(numberOfDates).map(_.toInt) else header.takeRight(numberOfDates).map(_.substring(1).toInt)
    val dates: Seq[Int] = if(isMariusFile) header.takeRight(6).map(_.toInt) else getdates(datesFile)

    //val numberOfDates = if(isMariusFile) 6 else header.length - 4
    val numberOfDates = dates.length

    /** The cities with known populations for all dates */
    val startingCities: List[Seq[String]] = data.filter {_.takeRight(numberOfDates).forall(!_.isEmpty)}

    val realPopulations: Matrix = new Matrix(startingCities.toArray.map{_.takeRight(numberOfDates).map{_.toDouble}.toArray})

    /** Number of cities taken into account */
    val nbCities: Int = startingCities.size

    /** Read the position of the cities */
    val positions =  if(isMariusFile) startingCities.map { l => Position(l(5).toDouble, l(4).toDouble)} else startingCities.map { l => Position(0.0,0.0)}//startingCities.map { l => Position(l(2).toDouble, l(3).toDouble)}

    /** Number of column before the census columns */
    val columnsBeforeDates = header.size - numberOfDates


    val arokatos: Seq[String] = if(isMariusFile) startingCities.map(_(0)) else (0 to startingCities.length - 1 by 1).map{_.toString}
    val names: Seq[String] = if(isMariusFile) startingCities.map(_(1)) else (0 to startingCities.length - 1 by 1).map{_.toString}

    /** Latitudes of the cities in decimal degrees */
    //def latitudes = startingCities.map(_(4))
    val latitudes = positions.map{_.latitude}

    /** Longitudes of the cities in decimal degrees */
    //def longitudes = startingCities.map(_(5))
    val longitudes = positions.map{_.longitude}

    /** Populations of the cities at the first date */
    val initialPopulations = populations(dates.head,columnsBeforeDates,startingCities,dates).get

    /** Cities with hydrocarbons */
    val hydrocarbonDistribution: List[Boolean] = if(isMariusFile) startingCities.map(l => toBoolean(l(8))) else List.fill(startingCities.length)(false)

    /** Regions of the cities */
    val regions: Iterator[String] = if(isMariusFile) startingCities.map(_(2)).toIterator else List.fill(startingCities.length)("").toIterator

    /** A vector of boolean, true in case a city is a regional capital */
    val regionCapitals: Iterator[Boolean] = if(isMariusFile) startingCities.map(l => toBoolean(l(7))).toIterator else List.fill(startingCities.length)(false).toIterator

    /** A vector of boolean, true in case a city is a national capital */
    val nationalCapitals: Iterator[Boolean]  = if(isMariusFile) startingCities.map(l => toBoolean(l(9))).toIterator else List.fill(startingCities.length)(false).toIterator

    /** States cities belong to */
    val nations: Iterator[String] = if(isMariusFile) startingCities.map(_(3)).toIterator else List.fill(startingCities.length)("").toIterator

    MariusConfiguration(dates,realPopulations,positions,columnsBeforeDates,arokatos,names,latitudes,longitudes,initialPopulations,hydrocarbonDistribution,regions,regionCapitals,nationalCapitals,nations)
  }


  /** A converter function from string to boolean */
  private def toBoolean(s: String) =
    s match {
      case "1" => true
      case _ => false
    }
}