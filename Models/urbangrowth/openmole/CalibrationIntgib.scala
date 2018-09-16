import java.util.Date
import java.text.SimpleDateFormat

logger.level("FINE")

val growthRate = Val[Double]
val gravityWeight = Val[Double]
val gravityGamma = Val[Double]
val gravityDecay = Val[Double]
val gravityAlpha = Val[Double]

val id = Val[Int]

// Reporters
val logmse=Val[Double]
val mselog=Val[Double]

// config files
val popFile=Val[File]
val distFile=Val[File]
val dateFile=Val[File]

// Model
val model =
  ScalaTask(
    """
      |import urbangrowth.models.coevolution._
      |import urbangrowth.indicators._
      | val res = Coevolution(input.popFile, input.distFile, input.dateFile,input.growthRate,input.gravityWeight, input.gravityGamma, input.gravityDecay, 0.0, 1.0, 1.0).run()
      | val logmse = res.logmse
      | val mselog = res.mselog
    """.stripMargin
  ) set (
    //plugins += pluginsOf(urbangrowth),
    inputs += (popFile,distFile,dateFile,growthRate,gravityWeight,gravityGamma,gravityDecay,id),
    outputs += (growthRate,gravityWeight,gravityGamma,gravityDecay,id),
    outputs += (logmse,mselog)
  )
val modelCapsule = Capsule(model)

val fileSetting = ExplorationTask(
  (popFile in Seq( workDirectory / "data" / "FR_pops.csv")) x
  (distFile in Seq(workDirectory / "data"/ "FR_dist.csv")) x
  (dateFile in Seq(workDirectory / "data" / "FR_dates.csv")) x
  (id in Seq(1))
) set(
  inputs += (growthRate,gravityWeight,gravityGamma,gravityDecay),
  outputs += (growthRate,gravityWeight,gravityGamma,gravityDecay)
)

val eval = fileSetting -< model


val model = "intgib"
val datestr = (new SimpleDateFormat("yyyyMMdd")).format(new Date()).toString
val purpose = "TEST"
val resdir = model+"_"+purpose+"_"+datestr

val island =
  NSGA2Evolution(
    genome =
      Seq(
        growthRate in Range(0.0, 0.05),
        gravityWeight in Range(0.0,0.05),
        gravityGamma in Range(0.1,10.0),
        gravityDecay in Range(0.1,20000.0)
      ),
    objectives = Seq(mselog,logmse),
    evaluation = eval,
    termination = 15 minutes,
    parallelism = 20,
    distribution = Island(20)
)


// Define the hook to save the results
val savePopulation = SavePopulationHook(island, workDirectory / "calib" / resdir)

//val grid = EGIEnvironment("vo.complex-systems.eu")
val local = LocalEnvironment(20)


//island on grid hook savePopulation
island on local hook savePopulation