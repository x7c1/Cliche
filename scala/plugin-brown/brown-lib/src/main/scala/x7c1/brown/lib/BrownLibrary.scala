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

    override val runsAfter: List[String] = List("namer")
    override val phaseName: String = BrownPlugin.this.name

    class BrownPhase(prev: Phase) extends StdPhase(prev){
      override def name = BrownPlugin.this.name
      override def apply(unit: global.CompilationUnit): Unit = {

        def traverse(tree: global.Tree, indent: String)(visitor: (global.Tree, String) => Unit): Unit = {
          tree.children.foreach{ t =>
            val space = indent + "  "
            visitor(t, space)
            traverse(t, space)(visitor)
          }
        }
        println(global.showRaw(unit.body))
        import global.{ClassDef, DefDef, Ident, PackageDef, TypeTree, AppliedTypeTree}
        traverse(unit.body, ""){
          case (p: PackageDef, indent) =>
            println(indent + p.name)
          case (t: Ident, indent) =>
          case (c: ClassDef, indent) =>
            println(indent + s"struct ${c.name} ${c.tparams.mkString(",")}")
          case (a: TypeTree, indent) =>
            a.original match {
              case o: AppliedTypeTree =>
                println(indent + s"${o.symbol.fullName} ${o.args}")
              case o: Ident =>

                println(indent + s"${o.symbol.fullName}!")
              case _ =>
            }
          case (d: DefDef, indent) =>
            println(indent + s"def ${d.name} ${d.tpt}")
          case tree =>
        }
        // trial to generate source by compiler-plugin
        File(s"target/${unit.source.file.name}.txt").writeAll(unit.body.toString())
      }
    }
  }
}
