
package urbangrowth.models

import Jama.Matrix

trait Model {

  //def setup(): Model

  def run(): Matrix

}
