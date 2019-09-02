
package urbangrowth.models.innovation

import Jama.Matrix
import java.io.File

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import urbangrowth.models.MacroModel
import urbangrowth.utils.io.FileUtils
import urbangrowth.indicators.Result
import urbangrowth.utils.math.MatrixUtils



case class Innovation(
                       /**
                         * Real population matrix
                         */
                       populationMatrix: Matrix,

                       /**
                         * Distance matrix
                         */
                      distanceMatrix: Matrix,

                       /**
                         * Dates
                         */
                       dates: Array[Double],

                       /**
                         * Model has its own rng
                         */
                       rng : Random,

                       /**
                         * Random seed
                         */
                       seed : Int,

                       /**
                         * Gibrat growth rate
                         */
                       growthRate: Double,

                       /**
                         * weight of innovation induced growth rate
                         */
                       innovationWeight: Double,

                       /**
                         * Decay of gravity interaction
                         */
                       gravityDecay: Double,

                       /**
                         * Decay of innovation diffusion
                         */
                       innovationDecay: Double,

                       /**
                         * Utility of the first innovation
                         */
                        innovationUtility: Double,

                       /**
                         * Growth of the innovation utility (default to 1.12)
                         */
                       innovationUtilityGrowth: Double,

                       /**
                         * Proportion of early adopters (defaults to 1%)
                         */
                       earlyAdoptersRate: Double,

                       /**
                         * City innovation hierarchy : exponent of the probability that a city introduces the new innovation
                         * (defaults to 1)
                         */
                      newInnovationHierarchy: Double,

                       /**
                         * Proportion of population at which new innovation emerges (defaults to 0.5)
                         */
                      newInnovationPopulationProportion: Double

                     ) extends MacroModel {

  override def run(): Result = Innovation.run(this)

  override def toString: String = "Innovation model with parameters"+
     "\n\tgrowthRate = "+growthRate+"\n\tinnovationWeight = "+innovationWeight+"\n\tgravityDecay = "+gravityDecay+
     "\n\tinnovationDecay = "+innovationDecay+"\n\tinnovationUtility = "+innovationUtility+"\n\tinnovationUtilityGrowth = "+innovationUtilityGrowth+
     "\n\tearlyAdoptersRate = "+earlyAdoptersRate+"\n\tnewInnovationHierarchy = "+newInnovationHierarchy+"\n\tnewInnovationPopulationProportion = "+newInnovationPopulationProportion

}


object Innovation {


  /**
    * Construct from setup files
    *
    * @param populationFile
    * @param distanceFile
    * @param datesFile
    * @param growthRate
    * @param innovationWeight
    * @param gravityDecay
    * @param innovationDecay
    * @param innovationUtility
    * @param innovationUtilityGrowth
    * @param earlyAdoptersRate
    * @param newInnovationHierarchy
    * @param newInnovationPopulationProportion
    * @return
    */
  def apply(populationFile: File,
            distanceFile: File,
            datesFile: File,
            seed: Int,
            growthRate: Double,
            innovationWeight: Double,
            gravityDecay: Double,
            innovationDecay: Double,
            innovationUtility: Double = 1.0,
            innovationUtilityGrowth: Double = 1.12,
            earlyAdoptersRate: Double = 0.01,
            newInnovationHierarchy: Double = 1.0,
            newInnovationPopulationProportion: Double = 0.5
           ): Innovation = {
    val populationMatrix = FileUtils.parseMatrixFile(populationFile)
    val distancesMatrix = FileUtils.parseMatrixFile(distanceFile)
    val dates = FileUtils.parseSimple(datesFile)

    val rng = new Random

    Innovation(populationMatrix,distancesMatrix,dates,rng,seed,growthRate,innovationWeight,gravityDecay,innovationDecay,innovationUtility,innovationUtilityGrowth,earlyAdoptersRate,newInnovationHierarchy,newInnovationPopulationProportion)
  }



  def run(model: Innovation): Result = {

    println("Running "+model.toString)

    import model._

    rng.setSeed(seed.toLong)

    val n = populationMatrix.getRowDimension
    val p = populationMatrix.getColumnDimension
    //println("n = "+n+" ; p = "+p)

    /**
      * Select a city hierarchically to population
      * @param currentPopulations
      * @param alpha
      * @return
      */
    def selectCityHierarchically(currentPopulations: Array[Double]): Int = {
      val r = rng.nextDouble
      val ptot = currentPopulations.map{math.pow(_,newInnovationHierarchy)}.sum
      Seq(Seq(0,currentPopulations.map{math.pow(_,newInnovationHierarchy)/ptot}.scanLeft(0.0)(_+_).indexWhere(_>r)).max,n-1).min
    }

    /**
      * Returns a Matrix for the new innov AND **Modifies in place the previous one**
      * @param previousInnovMatrix
      * @param currentPopulations
      * @param time
      * @return
      */
    def newInnovationMatrix(previousInnovMatrix: Matrix,currentPopulations: Array[Double],time: Int): Matrix = {
      val innovativeCityIndex: Int = selectCityHierarchically(currentPopulations)
      println("Innovative city : "+innovativeCityIndex)
      val diffrates: Matrix = new Matrix(n,p)
      diffrates.set(innovativeCityIndex,time,earlyAdoptersRate)
      previousInnovMatrix.set(innovativeCityIndex,time,previousInnovMatrix.get(innovativeCityIndex,time)-earlyAdoptersRate)
      diffrates
    }

    val inds = (0 to n - 1 by 1).toArray

    val res = new Matrix(n, p)
    res.setMatrix(0, n - 1, 0, 0, populationMatrix.getMatrix(0, n - 1, 0, 0))
    var currentPopulations: Array[Double] = populationMatrix.getMatrix(0, n - 1, 0, 0).getArray.flatten

    val gravityDistanceWeights = new Matrix(distanceMatrix.getArray().map { _.map { d => Math.exp(-d / gravityDecay) } })
    val innovationDistanceWeights = new Matrix(distanceMatrix.getArray().map { _.map { d => Math.exp(-d / innovationDecay) } })

    val innovationUtilities: ArrayBuffer[Double] = new ArrayBuffer[Double]
    innovationUtilities.append(innovationUtility,innovationUtility*innovationUtilityGrowth)
    val innovationProportions: ArrayBuffer[Matrix] = new ArrayBuffer[Matrix]
    // the first innovation is already in one city on top of a background archaic technology (can be replaced by the second at the first step, consistent as assumed as coming from before the simulated period)
    val archaicTechno = new Matrix(n,p);archaicTechno.setMatrix(0, n - 1, 0, 0,new Matrix(n,1,1.0))
    innovationProportions.append(archaicTechno)
    innovationProportions.append(newInnovationMatrix(archaicTechno,currentPopulations,time=0))

    for (t <- 1 to p - 1) {

      val delta_t = dates(t) - dates(t - 1)

      val totalpop = currentPopulations.sum

      /**
        * 1) diffuse innovations
        */
      val tmplevel: Array[Array[Double]] = innovationProportions.zip(innovationUtilities).map{case (m,u)=>
        new Matrix(Array(m.getMatrix(inds,Array(t-1)).copy.getArray.flatten.zip(currentPopulations).map{case(w,d)=> math.pow(w*d,u)})).times(innovationDistanceWeights).getArray.flatten
      }.toArray
      val cumtmp: Array[Double] = tmplevel.foldLeft(Array.fill(n)(0.0)){case (a1,a2)=>a1.zip(a2).map{case(d1,d2)=>d1+d2}}
      val deltaci: Array[Array[Double]] = tmplevel.map{_.zip(cumtmp).map{case (d1,d2)=>d1 / d2}}
      innovationProportions.zip(deltaci).foreach{case (innovmat,cityprops)=>
        innovmat.setMatrix(inds,Array(t),new Matrix(Array(cityprops)).transpose())
      }
      // compute macro adoption levels
      val macroAdoptionLevels: Array[Double] = innovationProportions.map{
        _.getMatrix((0 to n - 1 by 1).toArray,Array(t)).copy.getArray.flatten.zip(currentPopulations).map{case(w,d)=>w*d}.sum
      }.map{_ / totalpop}.toArray


      /**
        * 2) Update populations
        */
      val technoFactor: Array[Double] = innovationProportions.zip(macroAdoptionLevels).map{case(m,phi)=>
        m.getMatrix(inds,Array(t)).copy.getArray.flatten.map{math.pow(_,phi)}
      }.toArray.foldLeft(Array.fill(n)(1.0)){case(a1,a2)=>a1.zip(a2).map{case(d1,d2)=> d1*d2}}

      var diagpops = MatrixUtils.diag(currentPopulations).times(1 / totalpop)
      val potsgravity = diagpops.times(MatrixUtils.diag(technoFactor)).times(gravityDistanceWeights).times(diagpops)
      val meanpotgravity = potsgravity.getArray().flatten.sum / (n * n)

      val prevpop = MatrixUtils.colMatrix(currentPopulations)
      res.setMatrix(0, n - 1, t, t,
        prevpop.plus(prevpop.arrayTimes(potsgravity.times(new Matrix(n, 1, 1)).times(innovationWeight / (n * meanpotgravity)).plus(new Matrix(n, 1, growthRate)).times(delta_t)))
      )
      currentPopulations = res.getMatrix(inds,Array(t)).copy.getArray.flatten

      /**
        * 3) create a new innovation if needed
        */
      val latestInnovAdoption = innovationProportions.last.getMatrix(inds,Array(t)).getArray.flatten.zip(currentPopulations).map{case (w,p)=>w*p}.sum / currentPopulations.sum
      if (latestInnovAdoption > newInnovationPopulationProportion) {
        val newutility = innovationUtilities.last * innovationUtilityGrowth
        innovationUtilities.append(newutility)
        innovationProportions.append(newInnovationMatrix(innovationProportions.last,currentPopulations,time = t))
      }

    }

    val totalpop = currentPopulations.sum
    val macroAdoptionLevels: Array[Double] = innovationProportions.map{
      _.getMatrix((0 to n - 1 by 1).toArray,Array(p-1)).copy.getArray.flatten.zip(currentPopulations).map{case(w,d)=>w*d}.sum
    }.map{_ / totalpop}.toArray

    println("Innovations introduced : "+innovationProportions.length)
    println("Macro adoption levels : "+macroAdoptionLevels.mkString(","))

    Result(model.populationMatrix,res)
  }



}


