
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
  extension (s: String)
    def unary_- = "-" + s

  //val ctx: Setr = "<5 1 3>"//"<0 <0 4> 2 <3> <2 3>>"
  val show  = "dn"   // prints the context
  val clear = "([r]" //) empties context
  val unionWithWrapped = "ua"
  inline def ifmap(cond: String)(f: String) = s"($cond{$f}" //)
  inline def map(f: String) = ifmap("<>")(f) // ctx.map(f) where f is a function
  inline def filter(cond: String) = map("u") ++ -ifmap(s"r$cond")(clear) ++ "r" // ctx.filter(cond)

  // Bools:
  val negate = "u-1" // if ctx == 0 then 1 else if ctx == 1 then 0
  val isNonEmpty = map(clear) // booleifies ctx: 0 => 0   everything else => 1
  val isEmpty = isNonEmpty ++ negate
  val contains0 = s"({$clear 0} -<<>>" //)
  inline def notEquals(const: String) = s"u -$const $isNonEmpty"
  inline def equals(const: String) = notEquals(const) ++ negate
  inline def exists(cond: String) = filter(cond) ++ isNonEmpty
  inline def contains(const: String) = exists(equals(const)) // if ctx contains const then 1 else 0

  // pairs: // (a,b) = { {{},{a}}, {{b}} }
  val toSelfPair = "uuu0ua-0" ++ ifmap(contains0)("r0") // ctx => (ctx, ctx) = { {0,{ctx}}, {{ctx}} } // explanation: ctx -uuu-> {{ctx}} -0-> {0,{{ctx}}} -ua-> {{0,{{ctx}}},0,{{ctx}}} --0-> {{0,{{ctx}}},{{ctx}}} -if(contains0)then(r0)-> {{0,{ctx}},{{ctx}}}
  val toPairEmptySelf = "uuu<<><0>>"
  val toPairSelfEmpty = "uu0u<<0>>"

  val getSecond = ifmap(contains0)(clear) ++ "rrr"
  val getFirst = -getSecond

  inline def mapBoth(f: String) = map(map(map(f)))
  inline def mapFirst(f: String) = ifmap(contains0)(map(map(f)))
  inline def mapSecond(f: String) = -mapFirst(f)

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