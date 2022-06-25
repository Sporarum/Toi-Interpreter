import scala.util.parsing.combinator._
import scala.annotation.tailrec
import scala.util.parsing.combinator.token.Tokens


object Parser extends RegexParsers {

  def program: Parser[Program] = (instruction).*
  def instruction: Parser[Instruction] = add | remove | foreach | whileDo | newline | printContext | semicrement | decrement | singletonWrap

  def add:     Parser[Add]          =        set  ^^ { Add(_) }
  def remove:  Parser[Remove]       = "-" ~> set  ^^ { Remove(_) }

  def foreach: Parser[Foreach] = "-".? ~ "(" ~ program ~ "{" ~ program <~ "}" ^^ { case op ~ _ ~ p ~ _ ~ q => Foreach(p, q, op.isDefined) }
  def whileDo: Parser[WhileDo] = "-".? ~ "(" ~ program ~ "[" ~ program <~ "]" ^^ { case op ~ _ ~ p ~ _ ~ q => WhileDo(p, q, op.isDefined) }

  def newline       = "n" ^^^ Newline
  def printContext  = "d" ^^^ PrintContext
  def semicrement   = "a" ^^^ Semicrement
  def decrement     = "r" ^^^ Decrement
  def singletonWrap = "u" ^^^ Wrap


  def set:  Parser[Setr]       = manualSet | natural | pair

  def manualSet:  Parser[Setr] = "<" ~> set.* <~ ">"  ^^ { l => Setr(l.toSet) }
  def natural:    Parser[Setr] = number               ^^ { Setr.fromNat(_) }
  def pair:       Parser[Setr] = "(" ~> set ~ set <~ ")" ^^ { case a ~ b => Setr.fromPair(a, b) }

  def number: Parser[Nat]      = """(0|[1-9]\d*)""".r ^^ { _.toInt }
}

/*
abstract sealed class Instruction

case class  Add(s: Setr)    extends Instruction
case class  Remove(s: Setr) extends Instruction
case object Newline         extends Instruction
case object PrintContext    extends Instruction
case object Increment       extends Instruction
case object Decrement       extends Instruction
case object SingletonWrap   extends Instruction
*/