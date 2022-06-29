
import scala.compiletime.error

extension (s: String)
  def unary_- = "-" + s

object General:
  val show  = "dn"   // prints the context
  val clear = "([r]" //) empties context
  val unionWithWrapped = "ua"
  inline def ifmap(cond: String)(f: String) = s"($cond{$f}" //)
  inline def ifthen(cond: String)(f: String) = "u" ++ ifmap(cond)(f) ++ "r" // if cond(ctx) then f(ctx)
  inline def map(f: String) = ifmap("<>")(f) // ctx.map(f) where f is a function
  inline def filter(cond: String) = map("u") ++ -ifmap(s"r$cond")(clear) ++ "r" // ctx.filter(cond)

object Bool:
  import General._
  val negate = "u-1" // if ctx == 0 then 1 else if ctx == 1 then 0
  val isNonEmpty = map(clear) // booleifies ctx: 0 => 0   everything else => 1
  val isEmpty = isNonEmpty ++ negate
  val contains0 = s"({$clear 0} -<<>>" //)
  inline def notEquals(const: String) = s"u -$const $isNonEmpty"
  inline def equals(const: String) = notEquals(const) ++ negate
  inline def exists(cond: String) = filter(cond) ++ isNonEmpty
  inline def contains(const: String) = exists(equals(const)) // if ctx contains const then 1 else 0
  inline private def _2op(op: String)(cond1: String, cond2: String) = Pair.fromSelfSelf ++ Pair.mapFirst(cond1) ++ Pair.mapSecond(cond2) ++ op
  inline def and(cond1: String, cond2: String) = _2op(Pair.and)(cond1,cond2)
  inline def nand(cond1: String, cond2: String) = _2op(Pair.nand)(cond1,cond2)
  inline def or(cond1: String, cond2: String) = _2op(Pair.or)(cond1,cond2)
  inline def nor(cond1: String, cond2: String) = _2op(Pair.nor)(cond1,cond2)
  inline def xor(cond1: String, cond2: String) = _2op(Pair.xor)(cond1,cond2)
  
object Pair: // (a,b) = { {0,{a}}, {{b}} }
  import General._ ; import Bool._
  val _wrapFirst = "uu0u" // ctx => { {0,{ctx}} } //explanation: ctx -uu-> {{ctx}} -0-> {0,{ctx}} -u-> {{0,{ctx}}}
  val _wrapSecond = "uuu" // ctx => {{{ctx}}}

  val fromSelfSelf = _wrapFirst ++ "a-0" ++ -General.ifmap(contains0)("u") // ctx => (ctx, ctx) = { {0,{ctx}}, {{ctx}} } // explanation: ctx -wrapFirst-> {{0,{ctx}}} -a-> {{0,{{ctx}}},0,{{ctx}}} --0-> {{0,{{ctx}}},{{ctx}}} -if(contains0)then(r0)-> {{0,{ctx}},{{ctx}}}
  val fromEmptySelf = _wrapSecond ++ "<<><0>>"
  val fromSelfEmpty = _wrapFirst ++ "<<0>>"

  val toSet = "rr" // (a,b) => {a,b} // explanation: { {0,{a}}, {{b}} } -r-> { 0,{a},{b} } -r-> {a,b}
  val union = toSet + "r" // (a,b) => {a,b} // explanation: (a,b) -toSet-> {a,b} -r-> a union b
  val or = union
  val nor = or ++ negate
  val nand = mapBoth(negate) ++ or
  val and = nand ++ negate
  val xor = fromSelfSelf ++ mapFirst(nand) ++ mapSecond(or) ++ and

  val getSecond = General.ifmap(contains0)(clear) ++ union // first component will be the empty set
  val getFirst = -getSecond

  // TODO: find more efficient way ?
  val swap = mapSecond(_wrapFirst) ++ mapFirst(_wrapSecond) ++ union

  inline def mapBoth(f: String) = map(map(map(f)))
  inline def mapFirst(f: String) = General.ifmap(contains0)(map(map(f)))
  inline def mapSecond(f: String) = -mapFirst(f)

  inline def ifmap(cond: String)(f: String) = mapBoth(ifthen(cond)(f))


class Tupler[N <: Int & Singleton](val n: N):
  //inline if n < 2 then error("Tuple with less than 2 elements") // inline if can only be used in an inline method
  if n < 2 then throw new Exception("Tuple with less than 2 elements")
  inline def filledWithContext: String        = Tupler.filledWithContext(n)
  inline def get(i: Int): String              = Tupler.get(n)(i)
  inline def mapAt(i: Int)(f: String): String = Tupler.mapAt(n)(i)(f)
  inline def map(f: String): String        = Tupler.map(n)(f)
  inline def toSet: String                    = Tupler.toSet(n)
  inline def union: String                    = Tupler.union(n)
  inline def ifmap(cond: String)(f: String)   = this.map(General.ifthen(cond)(f))

object Tupler:
  private inline def check(n: Int) = inline if n < 2 then error("Tuple with less than 2 elements")

  private inline def filledWithContext(n: Int): String = 
    check(n)
    inline if n==2 then 
      Pair.fromSelfSelf
    else
      Pair.fromSelfSelf ++ Pair.mapSecond(filledWithContext(n-1))

  private inline def get(n: Int)(i: Int): String = // 0 indexed !
    check(n)
    inline if n==2 then 
      inline if i==0 then Pair.getFirst else inline if i==1 then Pair.getSecond else error("get index is greater than Tuple size")
    else
      inline if i==0 then Pair.getFirst else Pair.getSecond ++ get(n-1)(i-1)

  private inline def mapAt(n: Int)(i: Int)(f: String): String = 
    check(n)
    inline if n==2 then
      inline if i==0 then Pair.mapFirst(f) else inline if i==1 then Pair.mapSecond(f) else error("mapAt index is greater than Tuple size")
    else
      inline if i==0 then Pair.mapFirst(f) else Pair.mapSecond(mapAt(n-1)(i-1)(f))

  private inline def map(n: Int)(f: String): String = 
    check(n)
    inline if n==2 then
      Pair.mapBoth(f)
    else
      Pair.mapFirst(f) ++ Pair.mapSecond(map(n-1)(f))

  private inline def toSet(n: Int): String = 
    check(n)
    inline if n==2 then
      Pair.toSet
    else
      Pair.mapFirst("u") ++ Pair.mapSecond(toSet(n-1)) ++ Pair.union     //(1 (2 3)) -> ({1} (2 3)) -> ({1} {2 3}) -> {1 2 3}

  private inline def union(n: Int): String = 
    check(n)
    inline if n==2 then
      Pair.union
    else
      Pair.mapSecond(union(n-1)) ++ Pair.union //(1 (2 3)) -> (1 2u3) -> 1u2u3
end Tupler