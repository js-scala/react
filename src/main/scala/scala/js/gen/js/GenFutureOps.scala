package scala.js.gen.js

import virtualization.lms.common._
import concurrent.{Promise, Future}
import scala.js.exp.FutureOpsExp

trait GenFutureOps extends GenEffect {
  val IR: FutureOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case PromiseNew(maybeA) =>
      maybeA.fold(emitValDef(sym, "{ v: null, cs: [] }"))(a => emitValDef(sym, "{ v: " + quote(a) + ", cs: [] }"))
    case PromisePut(p, v) =>
      val i = fresh[Int]
      val n = fresh[Int]
      stream.println("if (" + quote(p) + ".v !== null) {")
      stream.println("throw 'This Promise has already been completed';")
      stream.println("} else {")
      stream.println(quote(p) + ".v = " + quote(v) + ";")
      stream.println("for (var " + quote(i) + " = 0, " + quote(n) + " = " + quote(p) + ".cs.length ; " + quote(i) + " < " + quote(n) + " ; " + quote(i) + "++) {")
      stream.println(quote(p) + ".cs[" + quote(i) + "](" + quote(v) + ");")
      stream.println("}")
      stream.println("}")
      emitValDef(sym, quote(()))
    case PromiseFuture(p) =>
      emitValDef(sym, quote(p))
    case FutureForeach(p, f) =>
      stream.println("if (" + quote(p) + ".v === null) {")
      stream.println(quote(p) + ".cs.push(" + quote(f) + ");")
      stream.println("} else {")
      stream.println(quote(f) + "(" + quote(p) + ".v);")
      stream.println("}")
      emitValDef(sym, quote(()))
    case _ => super.emitNode(sym, rhs)
  }

}