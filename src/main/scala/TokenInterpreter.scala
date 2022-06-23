
import scala.annotation.tailrec

type WithSideEffects[T] = T

object Contexts:
  opaque type Context[T] = T

  object Context:
    def apply[T](e: T): Context[T] = e

  extension[T] (ctx: Context[T])
    def elem: T = ctx
import Contexts.*

object TokenInterpreter {
  // Assumes failing c's should remain unchanged (they could also get removed)
  val failingCsShouldGetDropped = false

  def eval(program: Program): WithSideEffects[Setr] = eval(program, Setr.empty)

  @tailrec
  def eval(program: Program, ctx: Setr): WithSideEffects[Setr] =
    program match
      case List() => ctx
      case instruction :: rest => 
        eval(rest, ctx = bigStep(instruction, ctx))

  def bigStep(instruction: Instruction, ctx: Setr): Setr = instruction match

    case Foreach(condition: Program, function: Program, emptyIsSucess: Boolean) =>
      val protoctx = ctx.map{ c =>
        given Context[Setr] = Context(c)

        val condRes = eval(condition, ctx = c)
        if condRes.isEmpty == emptyIsSucess then //equivalent to: (empty && emptyIsSuccess) || (!empty && !emptyIsSuccess)
          val funRes = eval(function, ctx = c)
          funRes
        else
          if failingCsShouldGetDropped then
            Setr.empty
          else
            c
      }
      Setr(protoctx)
    
    case WhileDo(condition: Program, function: Program, emptyIsSucess: Boolean) =>
      @tailrec
      def rec(currCtx: Setr): WithSideEffects[Setr] =
        given Context[Setr] = Context(currCtx)

        val condRes = eval(condition, currCtx)
        if condRes.isEmpty == emptyIsSucess then
          val funRes = eval(function, currCtx)

          rec(funRes)
        else
          currCtx
      
      rec(ctx)
    
    case Newline         =>
      println()
      ctx
    case PrintContext    =>
      print(ctx.toString)
      ctx

    case Add(s: Setr)    => ctx + s
    case Remove(s: Setr) => ctx - s
    case Increment       => ctx.increment
    case Decrement       => ctx.decrement
    case Wrap            => ctx.wrap
  
    /*
  def smallStepEval(program: Program)(using ctx: Context[Setr]): (Setr, String) = // TODO: Rework as WithSideEffects[ Setr ] ?
    //TODO
    ???

  def step(instruction: Instruction)(using _ctx: Context[Setr]): (Setr, String) = 
    val ctx: Setr = _ctx.elem

    instruction match

    case Foreach(condition: Program, function: Program, emptyIsSucess: Boolean) =>
      ???
    case WhileDo(condition: Program, function: Program, emptyIsSucess: Boolean) =>
      ???

    case Add(s: Setr)    =>
      ctx + s
    case Remove(s: Setr) =>
      ctx - s

    case Newline         =>
      (ctx, "\n")
    case PrintContext    =>
      (ctx, ctx.toString)
    case Increment       =>
      ctx.increment
    case Decrement       =>
      ctx.decrement
    case Wrap            =>
      ctx.wrap
  */
}
