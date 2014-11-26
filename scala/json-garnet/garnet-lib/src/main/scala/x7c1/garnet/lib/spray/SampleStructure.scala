package x7c1.garnet.lib.spray

trait SampleStructure {
  def x: Float
  def y: String
}

object SampleStructure {
  def apply(x: Float, y: String): SampleStructure =
    new SampleStructureImpl(x, y)

  private class SampleStructureImpl(
    override val x: Float,
    override val y: String) extends SampleStructure
}
