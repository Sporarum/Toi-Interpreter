import scala.annotation.tailrec

val printAsOrdinals = true
val printAsTuple    = true
val printAsPair     = true
val printAsList     = true
val printAsTape     = true

def optionIf[T](cond: Boolean)(op: => Option[T]): Option[T] = if cond then op else None

type Nat = Int

val Empty = Setr(Set.empty)
val Nil = Empty

object Singleton:
  def unapply(s: Setr): Option[Setr] = s.asSingletonOption

object Setr:
  def apply(setrs: Setr*): Setr = Setr(setrs.toSet)
  def apply(s: Set[Setr]): Setr = new Setr(s)()
  def empty = Setr(Set.empty)(natOp = Option(0))
  def fromNat(n: Nat): Setr = 
    if n == 0 then empty 
    else Setr.fromNat(n-1).increment
  private def _fromPair(a: Setr, b: Setr): Set[Setr] = Set( Setr(Empty, a.wrap), b.wrap.wrap )
  def fromPair(a: Setr, b: Setr): Setr = Setr(_fromPair(a,b))(pairOp = Some( (a,b) )) // this == (a,b) <=> this == { {{},{a}}, {{b}} }
  def fromTuple(elems: Setr*): Setr = elems match
    case Seq() => throw new Exception("Setr.fromTuple needs at least one element")
    case Seq(a) => a
    case a :: rest => 
      val nRest = fromTuple(rest: _*)
      Setr(_fromPair(a,nRest))(pairOp = Some(a, nRest), tupleOp = Some(a +: nRest.asTuple))
  
  def fromList(l: Setr*): Setr = 
    val tupleElems = l.map(_.wrap) :+ Nil
    fromTuple(tupleElems: _*)

  def fromTape(l: List[Setr], h: Setr, r: List[Setr]): Setr = 
    val a = h +: l.reverse
    fromPair( fromList(a: _*), fromList(r: _*) )

case class Setr(s: Set[Setr])(natOp: Option[Nat] = None, tupleOp: Option[List[Setr]] = None, pairOp: Option[(Setr,Setr)] = None):
  /**
   * Is this set a Von Neumann ordinal ? (well technically only works on naturals, hence Nat)
   * if yes, returns Some of that ordinal
   * else returns None
  **/
  lazy val asNatOption: Option[Nat] = natOp orElse computeAsNatOption

  /**
   * Is this set a singleton ?
   * if yes, returns Some of the contained element
   * else returns None
  **/
  lazy val asSingletonOption: Option[Setr] = computeAsSingletonOption
  
  /**
   * Is this set a pair ? assuming (a,b) = { {{},{a}}, {{b}} }
   * if yes, returns Some of that pair
   * else returns None
  **/
  lazy val asPairOption: Option[(Setr, Setr)] = pairOp orElse computeAsPairOption

  /**
   * this set as a tuple, assuming (a,b,c,...) = (a,(b,c,...)), and (this) otherwise
  **/
  lazy val asTuple: List[Setr] = tupleOp getOrElse computeAsTuple
  lazy val asTupleOption: Option[List[Setr]] = tupleOp orElse computeAsTupleOption

  // List() = 0, List(a,b, ...)  = (<a>,List(b,...)) => all elements get singleton wrapped
  lazy val asListOption: Option[List[Setr]] = computeAsListOption

  lazy val asTapeOption: Option[(List[Setr], Setr, List[Setr])] = computeAsTapeOption

  def wrap: Setr = Setr(Set(this)) // creates: {this}
  def increment: Setr = asNatOption match
    case Some(n) => Setr( s + this )(natOp = Some(n+1))
    case None => this.wrap union this //equivalent to this.wrap.semicrement
  def semicrement: Setr = this union manualDecrement
  def decrement: Setr = asNatOption match
    case Some(0) => this
    case Some(n) => s.maxBy(_.asNatOption.get) // get will always suceed: s is a nat, it contains only nats ! (and it contains something because it's not 0)
    case None => manualDecrement

  private def manualDecrement: Setr = Setr(s.flatMap(setr => setr.s))

  def plainString: String = this.s.map(_.plainString).mkString("<"," ",">")
  override def toString: String = {
    optionIf(printAsOrdinals)(this.asNatOption.map(_.toString)) orElse
    optionIf(printAsTape    )(this.asTapeOption.map( (l,h,r) => l.mkString("<{"," "," [") ++ h.toString ++ r.mkString("] "," ","}>") )) orElse
    optionIf(printAsList    )(this.asListOption.filter(_.nonEmpty).map(_.mkString("<["," ","]>"))) orElse
    optionIf(printAsTuple   )(this.asTupleOption.map(_.mkString("("," ",")"))) orElse
    optionIf(printAsPair    )(this.asPairOption.map(_.toString)) orElse
    Some( s.mkString("<"," ",">") )
  }.get

  // Usual Set methods:

  def union(that: Setr): Setr = Setr(this.s union that.s)
  def partition(p: Setr => Boolean): (Setr, Setr) = { val (t, f) = s.partition(p); (Setr(t), Setr(f)) }

  def +(that: Setr): Setr = incl(that)
  def incl(that: Setr): Setr = Setr(s incl that)
  def -(that: Setr): Setr = excl(that)
  def excl(that: Setr): Setr = Setr(s excl that)

  def map(f: Setr => Setr): Setr = Setr(s.map(f))
  def flatMap(f: Setr => IterableOnce[Setr]): Setr = Setr(s.flatMap(f))

  export s.{isEmpty, nonEmpty, size, contains}

  // computeAsXOption:
  
  /**
   * Checks if this set is a Nat, i.e. if it contains only Nats from 0 to n
   * if it is, returns Some(n+1)
   * else returns None
  **/
  private def computeAsNatOption: Option[Nat] = 
    val z: Option[(Nat, List[Nat])] = Some( (-1, List()) ) // List assumed to be always sorted (from min to max) ! see nUngrouped
    val res = s.map(_.asNatOption).foldLeft(z){
      case (acc, None) => None
      case (None, curr) => None
      case (Some((maxGrouped, ungrouped)), Some(n)) if n == maxGrouped + 1 =>
        @tailrec
        def rec(maxGrouped: Nat, ungrouped: List[Nat]): (Nat, List[Nat]) =
          val incd = maxGrouped+1
          ungrouped.headOption match
            case Some(`incd`) => rec(incd, ungrouped.tail)
            case _ => (maxGrouped, ungrouped)
        end rec

        Some(rec(n, ungrouped))

      case (Some((maxGrouped, ungrouped)), Some(n)) =>
        val nUngrouped = (n +: ungrouped).sorted
        Some( (maxGrouped, nUngrouped) )
    }
    res match{
      case Some(n, List()) => Some(n+1) // The set contains only consecutive nats <=> it is the next nat
      case _ => None
    }

  private def computeAsSingletonOption: Option[Setr] = Option.when(this.size == 1)(s.head)

  private def computeAsPairOption: Option[(Setr, Setr)] = // this == (a,b) <=> this == { {{},{a}}, {{b}} }
    this.partition(_.contains(Empty)) match
      case (Singleton(protoA), Singleton(protoB)) => // {{{},{a}}},{{{b}}} => protoA = {{},{a}}, protoB = {{b}}
        def asSingletonSingleton(s: Setr) = s.asSingletonOption.flatMap(_.asSingletonOption)
        val aOption = asSingletonSingleton(protoA - Empty) //protoA - Empty = {{a}}
        val bOption = asSingletonSingleton(protoB)
        aOption zip bOption
      case _ => None
  
  private def computeAsTuple: List[Setr] = // this == (a,b,c, ...) <=> this == (a,(b,c,...)) and this = (this)
    asPairOption
      .map{ (a, rest) => a +: rest.asTuple }
      .getOrElse(List(this))
  
  private def computeAsTupleOption: Option[List[Setr]] = //only "real" tuples
    Some(asTuple).filter(_.size >= 2)

  private def computeAsListOption: Option[List[Setr]] = computeAsTupleOption.flatMap{ t => // List() = 0, List(a,b, ...)  = (<a>,List(b,...)) => all elements get singleton wrapped
    Option.when(t.init.forall(_.nonEmpty) && t.last.isEmpty){ 
      t.init.map(_.decrement)
    }
  } orElse Option.when(isEmpty)(List())

  private def computeAsTapeOption: Option[(List[Setr], Setr, List[Setr])] = computeAsPairOption.flatMap{ (_a,_b) => // <{a b [c] d}> = (<[c b a]> <[d]>)
    (_a.asListOption zip _b.asListOption).map{ (a,b) =>
      val (curr, init) = if a.isEmpty then (Empty, List()) else (a.head, a.tail.reverse)
      val tail = b
      (init, curr, tail)
    }
  }
  
