
package urbangrowth.models

import urbangrowth.indicators.Result

trait Model {

  //def setup(): Model

  def run(): Result

}
