package ale.app1.bar.v2

object BarService {
  def findBars(parameter: BarRequestParameter): Seq[Bar] = {
    Seq()
  }
}

trait BarRequestParameter {
  def names: Seq[String]
}

trait Bar {
  def barKeyName: String
  def barValue: String
}
