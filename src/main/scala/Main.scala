
import Parser._

def eval(s: String): WithSideEffects[Unit] = eval(s, Setr.empty)

def eval(s: String, ctx: Setr): WithSideEffects[Unit] = 
  parseAll(program, s) match
    case Success(tokens,_) => 
      val output = TokenInterpreter.eval(tokens, ctx)
      println(s"output:\nplain:\n${output.plainString}\ntoString:\n$output\n")
    case Failure(msg,_) => println(s"FAILURE: $msg")
    case Error(msg,_)   => println(s"ERROR: $msg")

def parseSet(s: String): Unit = parseAll(set, s) match
    case Success(matched,_) => println(s"plain:\n${matched.plainString}\ntoString:\n$matched")
    case Failure(msg,_) => println(s"FAILURE: $msg")
    case Error(msg,_) => println(s"ERROR: $msg")


@main def hello: Unit = 

  parseSet("< 2 3 <> >")

  parseSet("<<2><2 3>>")
  

  /*
  val nat = parseAll(set, "2").get//Setr.fromNat(10)
  println(s"$nat.increment:")
  val suc = nat.increment
  println(s"${suc.setString}\n${suc.natString}\n")
  println(s"$nat.decrement:")
  val dec = nat.decrement
  println(s"${dec.setString}\n${dec.natString}\n")
  */

  val ctx = Setr.fromNat(0)
  println(s"ctx: $ctx") 
  eval("uu0ua-0", ctx)
  //eval("<> ([(<>{d} uan ]")

