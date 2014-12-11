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

  private object Component extends PluginComponent with SampleInspector{
    override val global: Global = BrownPlugin.this.global
    override def newPhase(prev: Phase) = new BrownPhase(prev)

    override val runsAfter: List[String] = List("refchecks")
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
//        assert(1 == 3)
        println(global.showRaw(unit.body))
        import global.{AppliedTypeTree, ClassDef, DefDef, Ident, PackageDef, TypeTree}

//        println(unit.body.symbol.fullName)

//        unit.body.children.flatMap(_.children).flatMap(_.children).foreach{
//          case t: AppliedTypeTree =>
//            println(t.symbol)
//          case _ =>
//        }

//        Eval("println(1+3)")
//        return
        traverse(unit.body, ""){
          case (x, indent) =>
//            println(indent + x.symbol)
            x match {
              case x: ClassDef =>
                println("-----")
                val s: global.Type = x.tpe
//                Global.T
//                global.Type x.tpe
                println("Type:", x.symbol)
                println(x.symbol.enclosingPackage.fullNameString)
                println(x.tparams)

                if (x.tparams.isEmpty && x.symbol.owner.hasPackageFlag && x.mods.isTrait){
                  val sample = x.symbol.fullName
//                  println(x.mods.isTrait)
                  println(s"!??")
//                  Eval(s"""new SampleInspector""")
//                  Eval(s"""import $sample; println(new $sample{})""")
//                  Eval(s"""import x7c1.brown.app.Response;""")
                }
                x.impl.body.foreach {
                  case s: DefDef =>
                    println("name", s.name)
                    println(s.tpt)
                    val resultType = s.tpt.symbol.typeSignatureIn(s.tpt.symbol.tpe).resultType
                    println("result", resultType.typeArgs)
                    s.tpt match {
                      case s2: TypeTree =>
//                        println(s2.tpe.members)
                      case _ =>
                    }
//                    println(s.tpt)
                  case _ =>
                }
//                x.tpe.members.foreach{println}
                x.children.foreach{
                  case x: global.DefDef =>
//                    println(x.tpe)
//                    println(x.name)
                    println(x)
                  case s  =>
//                    println(s.symbol)
                }
                val typeSummary = buildFrom(x.tpe)
                println(typeSummary.fullName)

              case _ =>
            }

//            x.symbol
        }


//        return
        traverse(unit.body, ""){
          case (p: PackageDef, indent) =>
            println(indent + p.name)
          case (t: Ident, indent) =>
          case (c: ClassDef, indent) =>
            println(indent + s"struct ${c.name} ${c.tparams.mkString(",")}")
          case (a: TypeTree, indent) =>
//            println("---")

//            a.tpe.members.
//              filter(x => x.isMethod && x.isAbstract).
//              map(_.asMethod).
//              foreach{ m =>
//                println(">>", m.typeSignatureIn(a.tpe))
//              }

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

//        println(unit.)
        // trial to generate source by compiler-plugin
        File(s"target/${unit.source.file.name}.txt").writeAll(unit.body.toString())
      }
    }
  }
}
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

object Eval {

  def apply[A](string: String): A = {
    val toolbox = currentMirror.mkToolBox()
    val tree = toolbox.parse(string)
    toolbox.eval(tree).asInstanceOf[A]
  }

}

trait SampleInspector{
  val global: Global

  val nameFilter = ! (_: String).startsWith("scala.")

//  import global.{Type, Symbol}

  def findPackage(symbol: global.Symbol) =
    if (symbol.hasPackageFlag) Some(symbol.fullName) else None

  def buildFrom(target: global.Type): SaladType = {
    val fields = target.members.view.
      filter(x => nameFilter(x.owner.fullName)).
      filter(x => x.isMethod && x.isAbstract).
      map(_.asMethod).filter(_.paramLists.isEmpty).
      map{ method =>
        val resultType =  method.typeSignatureIn(target).resultType
        new SaladField(
          decodedName = method.name.decodedName.toString,
          resultType = buildFrom(resultType))
    }

//    println(target.members)
    new SaladType(
      packageName = findPackage(target.typeSymbol.owner),
      fullName = target.typeSymbol.fullName,
      typeArguments = target.typeArgs.map(buildFrom),
      members = fields.toList )
  }
}

class SaladType(
  /**
   * exists if enclosing symbol is package
   * otherwise none (e.g. if defined in object, trait, class)
   */
  val packageName: Option[String],
  val fullName: String,
  val typeArguments: Seq[SaladType],
  val members: Seq[SaladField]){

  lazy val typedName = {
    def expand(x: SaladType): String = x.fullName + {
      if (x.typeArguments.isEmpty) ""
      else x.typeArguments.map(expand).mkString("[", ",", "]")
    }
    expand(this)
  }
}
class SaladField(
  val decodedName: String,
  val resultType: SaladType)
