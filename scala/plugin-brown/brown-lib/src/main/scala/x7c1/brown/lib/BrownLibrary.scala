package x7c1.brown.lib

import scala.reflect.io.File
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.{Global, Phase}

object BrownLibrary {
  def createMessageFor(name: String) = s"hello, $name!"
}

class BrownPlugin(val global: Global) extends Plugin {

  override val name: String = "brown-plugin"
  override val description: String = "sample compiler plugin"
  override val components =  List[PluginComponent](Component)

  private object Component extends PluginComponent {
    override val global: Global = BrownPlugin.this.global
    override def newPhase(prev: Phase) = new BrownPhase(prev)
    override val runsAfter: List[String] = List("parser")
    override val phaseName: String = BrownPlugin.this.name

    class BrownPhase(prev: Phase) extends StdPhase(prev){
      override def name = BrownPlugin.this.name
      override def apply(unit: global.CompilationUnit): Unit = {
        println(unit.body)

        def traverse(tree: global.Tree)(visitor: global.Tree => Unit): Unit = {
          tree.children.foreach{ t =>
            visitor(t)
            traverse(t)(visitor)
          }
        }

        import global.{showRaw, Ident, ClassDef, DefDef}
        traverse(unit.body){
          case t: Ident =>
          case c: ClassDef =>
            println(showRaw(c))
          case d: DefDef =>
            println(d)
          case tree =>
        }
        // trial to generate source by compiler-plugin
        File(s"target/${unit.source.file.name}.txt").writeAll(unit.body.toString())
      }
    }
  }
}
