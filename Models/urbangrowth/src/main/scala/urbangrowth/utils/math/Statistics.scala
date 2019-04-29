package urbangrowth.utils.math

object Statistics {


  def rankSizeDistribution(size: Int,alpha: Double, pmax: Double): Vector[Double] = (1.0 to size by 1.0).map{i => pmax*math.pow(1/i,alpha)}.toVector


}
