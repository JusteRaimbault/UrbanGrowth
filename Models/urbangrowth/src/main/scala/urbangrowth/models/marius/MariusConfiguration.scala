
package urbangrowth.models.marius

import Jama.Matrix

case class MariusConfiguration(
                                dates: Seq[Int],
                                realPopulations: Matrix,
                                positions: Seq[Position],
                                columnsBeforeDates: Int,
                                arokatos: Seq[String],
                                names: Seq[String],
                                latitudes: Seq[Double],
                                longitudes: Seq[Double],
                                initialPopulations: Seq[Double],
                                hydrocarbonDistribution: List[Boolean],
                                regions: Iterator[String],
                                regionCapitals: Iterator[Boolean],
                                nationalCapitals: Iterator[Boolean],
                                nations: Iterator[String]
                              )



