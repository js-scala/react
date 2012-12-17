package scala.js

import virtualization.lms.common._
import concurrent.{Promise, Future}

// TODO Encode at the type level whether a promise has been completed or not
// TODO Reify Future operations
trait FutureOps { this: Base with IfThenElse =>

  def promise[A : Manifest]: Rep[Promise[A]]
  implicit class PromiseOps[A : Manifest](p: Rep[Promise[A]]) extends Serializable {
    def put(v: Rep[A]) = promise_put(p, v)
    def future = promise_future(p)
  }

  def promise_put[A : Manifest](p: Rep[Promise[A]], v: Rep[A]): Rep[Unit]
  def promise_future[A : Manifest](p: Rep[Promise[A]]): Rep[Future[A]]


  def future[A : Manifest](a: Rep[A]): Rep[Future[A]]
  implicit class FutureOps[A : Manifest](fa: Rep[Future[A]]) extends Serializable {
    def foreach(f: Rep[A] => Rep[Unit]): Rep[Unit] = future_foreach(fa, f)
    def map[B : Manifest](f: Rep[A] => Rep[B]): Rep[Future[B]] = {
      val pb = promise[B]
      for (a <- fa) pb.put(f(a))
      pb.future
    }
    def flatMap[B : Manifest](f: Rep[A] => Rep[Future[B]]): Rep[Future[B]] = {
      val pb = promise[B]
      for {
        a <- fa
        b <- f(a)
      } pb.put(b)
      pb.future
    }
    def withFilter(f: Rep[A] => Rep[Boolean]): Rep[Future[A]] = {
      val pa = promise[A]
      for (a <- fa) if (f(a)) pa.put(a)
      pa.future
    }
  }

  def future_foreach[A : Manifest](fa: Rep[Future[A]], f: Rep[A] => Rep[Unit]): Rep[Unit]
}

trait FutureOpsExp extends FutureOps with EffectExp { this: IfThenElse with TupledFunctions =>

  case class PromiseNew[A](value: Option[Exp[A]]) extends Def[Promise[A]]
  case class PromisePut[A](p: Exp[Promise[A]], v: Exp[A]) extends Def[Unit]
  case class PromiseFuture[A](p: Exp[Promise[A]]) extends Def[Future[A]]
  case class FutureForeach[A](fa: Exp[Future[A]], f: Exp[A => Unit]) extends Def[Unit]

  def promise[A : Manifest] = reflectEffect(PromiseNew[A](None))
  def promise_put[A : Manifest](p: Exp[Promise[A]], v: Exp[A]) = promise_putF[A].apply(p, v)
  def promise_future[A : Manifest](p: Exp[Promise[A]]) = PromiseFuture(p)

  def future[A : Manifest](a: Exp[A]) = (PromiseNew(Some(a)): Exp[Promise[A]]).future
  def future_foreach[A : Manifest](fa: Exp[Future[A]], f: Exp[A] => Exp[Unit]) = future_foreachF[A].apply(fa, f)

  // TODO Support reified polymorphic functions
  def promise_putF[A : Manifest]: Exp[((Promise[A], A)) => Unit] = { (p: Exp[Promise[A]], v: Exp[A]) =>
    reflectEffect(PromisePut(p, v))
  }
  def future_foreachF[A : Manifest]: Exp[((Future[A], A => Unit)) => Unit] = { (fa: Exp[Future[A]], f: Exp[A => Unit]) =>
    reflectEffect(FutureForeach(fa, f))
  }
}

trait JSGenFutureOps extends JSNestedCodegen {
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