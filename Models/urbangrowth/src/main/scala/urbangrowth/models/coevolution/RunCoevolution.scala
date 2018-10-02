package urbangrowth.models.coevolution

import java.io.File

import urbangrowth.indicators._
import urbangrowth.utils.io.FileUtils

object RunCoevolution {

  def run(args: Array[String]): Unit = {
    assert(args.length>=3,"Coevol model needs at least 1 parameters")
    val system = args(1)

    val res: Result = args.length match {
      case 3 =>  Coevolution(new File("data/processed/"+system+"_pops.csv"), null,null, new File("data/processed/"+system+"_dates.csv"),
        args(2).toDouble,0.0, 1.0, 1.0, 0.0, 1.0, 1.0).run()
      case 6 =>  Coevolution(new File("data/processed/"+system+"_pops.csv"), null,null, new File("data/processed/"+system+"_dates.csv"),
        args(2).toDouble,args(3).toDouble, args(4).toDouble,args(5).toDouble, 0.0, 1.0, 1.0).run()
    }

    FileUtils.exportCSV(new File("res/popsim.csv"),res.simulatedPopulation)
    FileUtils.exportCSV(new File("res/poptarget.csv"),res.simulatedPopulation)

  }

}