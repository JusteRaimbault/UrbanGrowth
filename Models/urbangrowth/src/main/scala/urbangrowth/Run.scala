
package urbangrowth

import models.marius.RunMarius
import models.coevolution.RunCoevolution

object Run extends App {

  assert(args.length > 0,"Provide at least a model to run")

  val modelName = args(0)

  modelName match {
    case "Marius" => RunMarius.run(args)
    case "Coevolution" => RunCoevolution.run(args)
    case _ => assert(1==0,"Unknown model")
  }

}