package scala.js.exp

import scala.virtualization.lms.common.{TupledFunctionsExp, IfThenElseExp, EffectExp}
import scala.concurrent.{Future, Promise}
import scala.js.language.FutureOps

trait FutureOpsExp extends FutureOps with EffectExp with IfThenElseExp with TupledFunctionsExp {

  case class PromiseNew[A](value: Option[Exp[A]]) extends Def[Promise[A]]
  case class PromisePut[A](p: Exp[Promise[A]], v: Exp[A]) extends Def[Unit]
  case class PromiseFuture[A](p: Exp[Promise[A]]) extends Def[Future[A]]
  case class FutureForeach[A](fa: Exp[Future[A]], f: Exp[A => Unit]) extends Def[Unit]

  def promise[A : Manifest] = reflectEffect(PromiseNew[A](None))

  def promise_put[A : Manifest](p: Exp[Promise[A]], v: Exp[A]) = {
    def put[A : Manifest] = fun { (p: Exp[Promise[A]], v: Exp[A]) =>
      reflectEffect(PromisePut(p, v))
    }
    put[A].apply(p, v)
  }

  def promise_future[A : Manifest](p: Exp[Promise[A]]) = PromiseFuture(p)

  def future[A : Manifest](a: Exp[A]) = (PromiseNew(Some(a)): Exp[Promise[A]]).future

  def future_foreach[A : Manifest](fa: Exp[Future[A]], f: Exp[A] => Exp[Unit]) = {
    def foreach[AA : Manifest] = fun { (fa: Exp[Future[AA]], f: Exp[AA => Unit]) =>
      reflectEffect(FutureForeach(fa, f))
    }
    foreach[A].apply(fa, f)
  }
}
