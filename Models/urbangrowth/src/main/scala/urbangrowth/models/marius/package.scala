package urbangrowth.models

package object marius {

  /**
    * State of the model
    *
    * @param step the step number
    * @param cities the state of the cities
    * @param network the interaction network
    */
  case class State(step: Int, cities: Seq[City], network: FullNetwork)

  /**
    * State of a city
    *
    * @param population the population of the city in thousands of inhabitants
    * @param wealth the wealth of the city
    */
  case class City(population: Double, wealth: Double)

  /** The matrix of distance between cities */
  type DistanceMatrix = Seq[Seq[Double]]

  /** A full network type containing the number of nodes */
  type FullNetwork = Int

  /** Utility methods to manipulate the full network*/
  implicit class FullNetworkDecorator(network: FullNetwork) {
    /**
      * Return all the node of the network except the node of index i
      *
      * @param i the node to exclude
      * @return a filtered sequence of nodes
      */
    def allExcept(i: Int) = (0 until i) ++ (i + 1 until network)

    /**
      * Neighbours of the node of index i via outgoing arcs
      * @param i index of the the node
      * @return the neighbourhood
      */
    def outNodes(i: Int) = allExcept(i)

    /**
      * Neighbours of the node of index i via incoming arcs
      * @param i index of the the node
      * @return the neighbourhood
      */
    def inNodes(i: Int) = allExcept(i)

    /**
      * Map a function on all arcs of the network
      *
      * @param f the function to map
      * @return a matrix containing the value of the function on each arc (i, j)
      */
    def mapNodes(f: (Int, Int) => Double): DenseMatrix =
      DenseMatrix(
        Array.tabulate(network, network) {
          (i, j) => if (i != j) f(i, j) else 0.0
        }
      )

  }

  /**
    * Position of a city in decimal degree
    *
    * @param longitude longitude of the city
    * @param latitude latitude of the city
    */
  case class Position(longitude: Double, latitude: Double)

  /**
    * Decoration of the iterator class to take while including the last one
    * @param i the iterator
    * @tparam A the iterator type
    */
  implicit class IteratorExtension[A](i: Iterator[A]) {
    def takeWhileInclusive(p: A => Boolean) = {
      val (a, b) = i.span(p)
      a ++ (if (b.hasNext) Some(b.next) else None)
    }
    def last = {
      def last[T](i: Iterator[T]): T = {
        val e = i.next()
        if (i.hasNext) last(i)
        else e
      }
      last(i)
    }
  }

}

