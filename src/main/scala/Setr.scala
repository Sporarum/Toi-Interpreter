import scala.annotation.tailrec

val printAsOrdinals = true
val printAsPair     = true

def optionIf[T](cond: Boolean)(op: => Option[T]): Option[T] = if cond then op else None

type Nat = Int

object Empty:
  def apply(): Setr = Setr(Set.empty)
  def unapply(s: Setr): Boolean = s.isEmpty

object Setr:
  def apply(s: Set[Setr]) = new Setr(s)()
  def empty = Setr(Set.empty)(natOp = Option(0))
  def fromNat(n: Nat): Setr = 
    if n == 0 then
      empty
    else
      Setr.fromNat(n-1).increment

case class Setr(s: Set[Setr])(natOp: Option[Nat] = None):
  /**
   * Is this set a Von Neumann ordinal ? (well technically only works on naturals, hence Nat)
   * if yes, returns Some of that ordinal
   * else returns None
   */
  lazy val asNatOption: Option[Nat] = natOp orElse computeAsNatOption

  /**
   * Is this set a singleton ?
   * if yes, returns Some of the contained element
   * else returns None
   */
  lazy val asUnwrappedOption: Option[Setr] = computeAsUnwrappedOption
  
  /**
   * Is this set a pair ? assuming (a,b) = {{a},{a,b}}
   * if yes, returns Some of that pair
   * else returns None
   */
  lazy val asPairOption: Option[(Setr, Setr)] = computeAsPairOption

  def wrap: Setr = Setr(Set(this)) // creates: {this}
  def increment: Setr = asNatOption match
    case Some(n) => Setr( s + this )(natOp = Some(n+1))
    case None => this union manualDecrement
  def decrement: Setr = asNatOption match
    case Some(0) => throw new Error("Cannot decrement 0")
    case Some(n) => s.maxBy(_.asNatOption.get) // get will always suceed: s is a nat, it contains only nats ! (and it contains something because it's not 0)
    case None => manualDecrement

  private def manualDecrement: Setr = Setr(s.flatMap(setr => setr.s))

  def plainString: String = this.map(_.plainString).mkString("<"," ",">")
  override def toString: String = {
      optionIf(printAsOrdinals)(this.asNatOption ) orElse
      optionIf(printAsPair    )(this.asPairOption)

    }.map(_.toString) getOrElse s.mkString("<"," ",">")

  // Usual Set methods:

  def union(that: Setr): Setr = Setr(this.s union that.s)
  //def partition(p: Setr => Boolean): (Setr, Setr) = { val (t, f) = s.partition(p); (Setr(t), Setr(f)) }

  def +(that: Setr): Setr = incl(that)
  def incl(that: Setr): Setr = Setr(s incl that)
  def -(that: Setr): Setr = excl(that)
  def excl(that: Setr): Setr = Setr(s excl that)

  export s.{map, isEmpty, size, contains}

  // computeAsXOption:
  
  /**
   * Checks if this set is a Nat, i.e. if it contains only Nats from 0 to n
   * if it is, returns Some(n+1)
   * else returns None
   */
  private def computeAsNatOption: Option[Nat] = 
    val z: Option[(Nat, List[Nat])] = Some( (-1, List()) ) // List assumed to be always sorted (from min to max) ! see nUngrouped
    val res = this.map(_.asNatOption).foldLeft(z){
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

  private def computeAsUnwrappedOption: Option[Setr] = Option.when(this.size == 1)(s.head)

  private def computeAsPairOption: Option[(Setr, Setr)] = // this == (a,b) <=> this == {{a},{a,b}}
    val singletonContents = this.map( subs => (subs,subs.asUnwrappedOption) ).collect{ case (wrapped,Some(unwrapped)) => (wrapped, unwrapped) }

    if singletonContents.size == 1 then // {a} should be the only singleton
      val (wrappedA, a) = singletonContents.head
      val (containsA, notContainsA) = s.partition( _ contains a ) // containsA = {{a},{a,b, ...}, ... }
      if notContainsA.isEmpty then
        if containsA.size == 1 then // this = {{a}} = {{a},{a,a}} = (a,a)
          Some( (a,a) )
        else
          Setr( containsA - wrappedA ).asUnwrappedOption.flatMap{ abSubset => // abSubset = {a,b, ...}
            (abSubset - a).asUnwrappedOption.map( b => (a, b) )
          }
      else
        None
    else
      None
