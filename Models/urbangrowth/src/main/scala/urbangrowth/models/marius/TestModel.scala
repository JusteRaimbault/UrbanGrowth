
package urbangrowth.models.marius

import java.io.File

object TestModel {

  def testModel: SimpleModel = SimpleModel(
    new File("data/processed/FR_dist.csv"),
    new File( "data/processed/FR_pops.csv"),
    new File("data/processed/FR_dates.csv"),
     0.34380934416368,
    1.00175638801509,
    1.07926078029368,
    0.672263161474518,
    0.380435604357353,
    1.08660127543912
  )

  /*
  def testModel : BonusFixedCostModel = BonusFixedCostModel(
  distanceMatrixFile = "data/marius/RU_dist.csv"
  ,

  bonusMultiplier = 197.948890779081
  ,
  fixedCost = 0.256524806806571
  ,
  distanceDecay = 0.672263161474518
  ,
  sizeEffectOnSupply = 1.00175638801509
  ,
  sizeEffectOnDemand = 1.07926078029368
  ,
  economicMultiplier = 0.34380934416368
  ,
  populationToWealthExponent = 1.08660127543912
  ,
  wealthToPopulationExponent = 0.380435604357353
    */


  //val path = new File("/tmp/mariusmodel_log.csv")
  //path.delete
  //val out = Resource.fromFile(path)
  //out.append("step, arokato, population, wealth \n")
  /*val populations: ArrayBuffer[Array[Double]] = new ArrayBuffer[Array[Double]]
  for {
    (s, i) <- TestModel.testModel
      .states.zipWithIndex
    ss <- s
  } {
    val cities = ss.cities
    populations.append(cities.map{_.population}.toArray)
    /*for {
      (city, arokato) <- (cities zip MariusFile.arokatos)
    } {
      def line = Seq(i, arokato, city.population, city.wealth)
      out.append(line.mkString("", ",", "\n"))
    }*/
    //val totalWealth = cities.map(_.wealth).sum
    //val totalPop = cities.map(_.population).sum
    //println(s"State $i, total wealth $totalWealth, total population $totalPop")
  }
  val popMatrix = new Matrix(populations.toArray)
  println(Indicators.logmse(popMatrix.transpose(),TestModel.testModel.modelConfiguration.startingCities
*/



}