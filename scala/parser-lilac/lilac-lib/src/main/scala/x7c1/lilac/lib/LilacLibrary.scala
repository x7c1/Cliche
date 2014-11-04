package x7c1.lilac.lib

import x7c1.lilac.lib.Modifiers.{Repeated, Optional, Required, RuleModifier}

import scala.util.parsing.combinator.RegexParsers

object LilacLibrary {
  def createMessageFor(name: String) = s"hello, $name!"
}

object Parser extends RegexParsers {

  val identifier = """[A-Za-z_]\w*""".r

  val assignNumber = "=" ~> """([1-9][0-9]*)""".r

  val field: Parser[Field] = {
    def field(rule: RuleModifier) = {
      val fieldType = identifier
      val fieldName = identifier
      val options ="""\[.+\]""".r.? // TODO

      rule.name ~> fieldType ~ fieldName ~ assignNumber ~ options <~ ";" ^^ {
        case x ~ y ~ z ~ _ => new Field(rule, x, y, z.toInt)
      }
    }
    field(Required) | field(Optional) | field(Repeated)
  }

  lazy val body: Parser[Body] =
    "{" ~> (field | message | enum).* <~ "}" ^^ {
      case nodes => new Body(nodes)
    }

  lazy val message: Parser[Message] =
    "message" ~> identifier ~ body ^^ {
      case n ~ b => new Message(name = n, body = b)
    }

  lazy val enum: Parser[Enum] = {
    val constant = identifier ~ assignNumber <~ ";" ^^ {
      case v ~ t => new EnumConstant(value = v, tagNumber = t.toInt)
    }
    "enum" ~> identifier ~ ("{" ~> constant.* <~ "}") ^^ {
      case n ~ c => new Enum(name = n, constants = c)
    }
  }

}

sealed trait Node

class Field(
  val fieldRule: RuleModifier,
  val fieldType: String,
  val fieldName: String,
  val tagNumber: Int ) extends Node

object Modifiers {
  sealed class RuleModifier {
    def name: String = toString.toLowerCase
  }
  case object Optional extends RuleModifier
  case object Repeated extends RuleModifier
  case object Required extends RuleModifier
}

class Body(val nodes: Seq[Node] = Seq())

class Message(
  val name: String,
  val body: Body) extends Node

class Enum(
  val name: String,
  val constants: Seq[EnumConstant] ) extends Node

class EnumConstant(
  val value: String,
  val tagNumber: Int)
