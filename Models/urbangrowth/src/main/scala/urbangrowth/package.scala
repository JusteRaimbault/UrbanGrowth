package object urbangrowth {



  type Populations = Array[Double]
  type Distances = Array[Array[Double]]



  sealed trait Logger

  case object Printer extends Logger
  case object DummyLogger extends Logger

  implicit val logger = Printer

  def log(msg: => String)(implicit logger: Logger) = {
    logger match {
      case Printer => println(msg)
      case DummyLogger =>
    }
  }


}

