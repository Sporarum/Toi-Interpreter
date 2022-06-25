
type Program = List[Instruction]

abstract sealed class Instruction

case class  Add(s: Setr)    extends Instruction
case class  Remove(s: Setr) extends Instruction

/**
 * For each c in ctx the Toi program cond is run with c as the context. 
 * If after running cond the context of cond is a nonempty* set, 
 *   B is run with c as context, 
 *   and c is replaced in ctx by the resulting context after running func. // Is this line meant to be indented ?
 * Changes to ctx are done after the for-each.
 */
case class  Foreach(condition: Program, function: Program, emptyIsSucess: Boolean) extends Instruction

/**
 *  While running cond with context ctx results in a nonempty* set, 
 *    run func with context ctx, changing ctx to func's result after running func.
 */
case class  WhileDo(condition: Program, function: Program, emptyIsSucess: Boolean) extends Instruction

case object Semicrement     extends Instruction
case object Decrement       extends Instruction
case object Wrap            extends Instruction
case object Newline         extends Instruction
case object PrintContext    extends Instruction
