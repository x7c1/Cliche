package x7c1.noir.lib

import scala.util.parsing.combinator.RegexParsers

object NoirLibrary {
  def createMessageFor(name: String) = s"hello, $name!"
}

object NoirParser extends RegexParsers {
  lazy val identifier = """[^(^)^,]*""".r

  lazy val tree: Parser[Tree] =
    identifier ~ body ^^ {
      case i ~ b => new Tree(name = i, nodes = b)
    }

  lazy val receiverTree =
    tree ~ ("." ~> identifier) ~ body ^^ { case t ~ i ~ b =>
      ReceiverTree(
        tree = t,
        message = Message(name = i, b))
    }

  lazy val leaf: Parser[Leaf] =
    """[^(^)^,]+""".r ^^ {
      case i => new Leaf(name = i)
    }

  lazy val node: Parser[Node] = receiverTree | tree | leaf

  lazy val body: Parser[Seq[Node]] =
    "(" ~> node.? ~ ("," ~> node).* <~ ")" ^^ {
      case head ~ tail => head.toSeq ++ tail
    }
}

sealed trait Node

case class Tree(
  name: String,
  nodes: Seq[Node]) extends Node

case class ReceiverTree(
  tree: Tree,
  message: Message) extends Node

case class Message(name: String, nodes: Seq[Node])

case class Leaf(name: String) extends Node

class NoirRenderer(indent: String){
  import scala.language.reflectiveCalls

  private type Noun = {
    def name: String
    def nodes: Seq[Node]
  }
  def render(node: Node, space: String): String = {
    def from(x: Noun) = {
      val body = x.nodes.map(render(_, indent + space)).mkString(",\n")
      s"${x.name}(\n$body)"
    }
    node match {
      case tree: ReceiverTree =>
        render(tree.tree, space) + "." + from(tree.message)
      case Tree(name1, Seq(Tree(name2, Seq(Leaf(leaf))))) =>
        space + s"$name1($name2($leaf))"
      case Tree(name, Seq(Leaf(leaf))) =>
        space + s"$name($leaf)"
      case tree: Tree => tree.nodes match {
        case Seq() =>
          space + tree.name + "()"
        case nodes =>
          space + from(tree)
      }
      case leaf: Leaf =>
        space + leaf.name
    }
  }
}

object NoirRenderer {
  def prettyPrint(node: Node) = {
    new NoirRenderer(indent = "  ").render(node, "")
  }
}
