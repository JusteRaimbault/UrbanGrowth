
package urbangrowth.test

import java.io.File

import Jama.Matrix


object Test extends App {

  TestModels.testMarius()

  //TestModels.testIntGib()

}



object TestModels {


  def testMarius(): Unit = {

    import urbangrowth.models.marius.TestModel
    import urbangrowth.models.marius.MariusFile

    //val path = new File("/tmp/mariusmodel_log.csv")
    //path.delete
    //val out = Resource.fromFile(path)
    //out.append("step, arokato, population, wealth \n")

    for {
      (s, i) <- TestModel.states.zipWithIndex
      ss <- s
    } {
      val cities = ss.cities
      /*for {
        (city, arokato) <- (cities zip MariusFile.arokatos)
      } {
        def line = Seq(i, arokato, city.population, city.wealth)
        out.append(line.mkString("", ",", "\n"))
      }*/
      val totalWealth = cities.map(_.wealth).sum
      val totalPop = cities.map(_.population).sum

      println(s"State $i, total wealth $totalWealth, total population $totalPop")
    }

  }


  /**
    * basic test of interaction gibrat
    */
  def testIntGib():Unit = {

    import urbangrowth.models.coevolution.Coevolution
    import urbangrowth.indicators.Indicators

    val pop = new File("data/coevolution/interactiongibrat/pop50.csv")
    val dists = new File("data/coevolution/interactiongibrat/dist50.csv")
    val fdists = new File("data/coevolution/interactiongibrat/distMat_Ncities50_alpha03_n03.csv")
    val fdates = new File("data/coevolution/interactiongibrat/dates.csv")
    val model = Coevolution(pop, dists, fdists, fdates,0.002, 0.01, 2.0, 100.0, 0.01, 2.0, 50.0)

    //var res: Matrix = null
    //for (decay <- 10.0 to 200.0 by 10.0) {
    //  println(decay)
    //res = Coevolution.run(0.002, 0.01, 2.0, 100.0, 2.0, 0.01, 2.0, 50.0)
    /*for (t <- 0 to res.getColumnDimension() - 1) { println(res.get(0, t)) }
    val real = InteractionModel.populationMatrix.copy()

    val logres = new Matrix(res.getArray().map { _.map { d => Math.log(d) } })
    val logreal = new Matrix(real.getArray().map { _.map { d => Math.log(d) } })
    val sqdiff = logres.minus(logreal).arrayTimes(logres.minus(logreal))
    println(sqdiff.getArray().flatten.sum)
    */

    val res = model.run()

    println(Indicators.logmse(res,model.populationMatrix))
    println(Indicators.mselog(res,model.populationMatrix))

  }


}
