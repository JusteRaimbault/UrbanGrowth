
package urbangrowth.test

import java.io.File

import Jama.Matrix
import urbangrowth.indicators.Indicators
import urbangrowth.models.innovation.Innovation

import scala.collection.mutable.ArrayBuffer


object Test extends App {

  TestModels.testInnovation()

  //TestModels.testMarius()

  //TestModels.testIntGib()

}



object TestModels {

  def testInnovation(): Unit = {
    val pop = new File("data/processed/FR_pops.csv")
    val dists = new File("data/processed/FR_dist.csv")
    val dates = new File("data/processed/FR_dates.csv")
    val rng = new scala.util.Random
    //val model = Innovation(pop,dists,dates,rng.nextInt,0.02,0.0001,200.0,50.0)
    val model = Innovation(pop,dists,dates,rng.nextInt,0.02,0.0001,200.0,50.0,1.0,2.0,0.5,5.0,0.1)
    val res = model.run()
    println(res)
  }


  def testMarius(): Unit = {

    import urbangrowth.models.marius.TestModel

    println(TestModel.testModel.run())

  }


  /**
    * basic test of interaction gibrat
    */
  def testIntGib():Unit = {

    import urbangrowth.models.coevolution.Coevolution
    import urbangrowth.indicators.Indicators

    //val pop = new File("data/coevolution/interactiongibrat/pop50.csv")
    val pop = new File("data/processed/FR_pops.csv")
    //val dists = new File("data/coevolution/interactiongibrat/dist50.csv")
    val dists = new File("data/processed/FR_dist.csv")
    //val fdists = new File("data/coevolution/interactiongibrat/distMat_Ncities50_alpha03_n03.csv")
    val fdists = null
    //val fdates = new File("data/coevolution/interactiongibrat/dates.csv")
    val fdates = new File("data/processed/FR_dates.csv")
    val model = Coevolution(pop, dists, fdists, fdates,0.02, 0.0015, 2.0, 500.0, 0.0, 2.0, 50.0)

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

    println(res)
    //println(Indicators.logmse(res,model.populationMatrix))
    //println(Indicators.mselog(res,model.populationMatrix))

  }


}
