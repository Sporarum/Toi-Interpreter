
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

  given Conversion[String, Setr] = parseSet(_)

  //val ctx: Setr = "<5 1 3>"//"<0 <0 4> 2 <3> <2 3>>"
  val show  = "dn"   // prints the context
  val clear = "([r]" // empties context
  val unionWithWrapped = "ua"
  inline def map(f: String) = s"(<>{$f}" //) ctx.map(f) where f is a function
  inline def filter(cond: String) = map("u") ++ s"-(r$cond{$clear}r" //) ctx.filter(cond)

  // Bools:
  val negate = "-([1]r" // if ctx == 0 then 1 else if ctx == 1 then 0
  val isNonEmpty = map(clear) // booleifies ctx: 0 => 0   everything else => 1
  val isEmpty = isNonEmpty ++ negate
  val contains0 = s"({$clear 0} -<<>>"
  inline def notEquals(const: String) = s"u -$const $isNonEmpty"
  inline def equals(const: String) = notEquals(const) ++ negate
  inline def exists(cond: String) = filter(cond) ++ isNonEmpty
  inline def contains(const: String) = exists(equals(const)) //) if ctx contains const then 1 else 0
  
  eval("uu0ua-0", "(2 3)")
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