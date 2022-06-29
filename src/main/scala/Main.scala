
import scala.compiletime.error
import Parser._

def eval(s: String): WithSideEffects[Unit] = eval(s, Setr.empty)

def eval(s: String, ctx: Setr): WithSideEffects[Unit] = 
  parseAll(program, s) match
    case Success(tokens,_) => 
      println(s"$s with context $ctx")
      val output = TokenInterpreter.eval(tokens, ctx)
      println(s"output set:\nplain:\n${output.plainString}\ntoString:\n$output\n")
    case Failure(msg,_) => println(s"FAILURE: $msg")
    case Error(msg,_)   => println(s"ERROR: $msg")

def parseSet(s: String): Setr = parseAll(set, s) match
    case Success(matched,_) => matched
    case Failure(msg,_) => throw new Exception(s"FAILURE: $msg")
    case Error(msg,_) => throw new Exception(s"ERROR: $msg")

def printSet(s: String): Unit =
  val matched = parseSet(s)
  println(s"parsed: $s\nplain:\n${matched.plainString}\ntoString:\n$matched")


@main def hello: Unit = 
  import General._
  import Bool._

  given Conversion[String, Setr] = parseSet(_)
  //eval("(<>[dnua]")    // prints all numbers
  //eval("r", ctx)       // converts pair to set (a,b) => {a,b}
  //wip val isSingleton = "(" ++ map("u") ++ "[]" //newPair = (a,b) = { {{},{x}}, {{y}} }
  // {a b} -> {{a} {b}}
  // {a}   -> {{a}}
  // yes: {a} {{}}   no: {a b} {a b ...} doesnt matter that much: {}
  //inline def and(cond1: String, cond2: String) = ???
  //inline def or(cond1: String, cond2: String) = ???
  //inline def onFirst(f: String) = ???

  /*
  inline def elvis(p: String, q: String) = // if ctx then p else q assuming ctx == 0 or ctx == 1

    ???
  */

/*
  todo:
    extract 1st/2nd element of pair
    detect singleton
    if
  identities:
    f == "u" ++ map(f) ++ "r"
    map(f) ++ filter(g) == filter(f ++ g)
    filter(true) == id
*/