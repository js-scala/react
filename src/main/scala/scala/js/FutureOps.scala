package scala.js

import virtualization.lms.common._
import concurrent.Future
import java.io.PrintWriter

trait FutureOps { this: Base =>

  trait FutureOpsBase[+A] {
    def map[B : Manifest](f: Rep[A] => Rep[B]): Rep[Future[B]]
    def flatMap[B : Manifest](f: Rep[A] => Rep[Future[B]]): Rep[Future[B]]
    def foreach(f: Rep[A] => Rep[Unit]): Rep[Unit]
    def withFilter(f: Rep[A] => Rep[Boolean]): Rep[Future[A]]
  }

  type FutureOpsCls[+A] <: FutureOpsBase[A]
  implicit def FutureOpsCls[A : Manifest](f: Rep[Future[A]]): FutureOpsCls[A]

  def future[A : Manifest](a: Rep[A]): Rep[Future[A]] // Wrong: should be a by-name parameter
}

trait FutureOpsExp extends FutureOps with EffectExp {

  case class FuturePure[A](a: Exp[A]) extends Def[Future[A]]
  case class FutureMap[A, B](a: Exp[Future[A]], x: Sym[A], b: Block[B]) extends Def[Future[B]]
  case class FutureFlatMap[A, B](a: Exp[Future[A]], x: Sym[A], b: Block[Future[B]]) extends Def[Future[B]]
  case class FutureForeach[A](a: Exp[Future[A]], x: Sym[A], b: Block[Unit]) extends Def[Unit]
  case class FutureFilter[A](a: Exp[Future[A]], x: Sym[A], b: Block[Boolean]) extends Def[Future[A]]

  def future[A : Manifest](a: Rep[A]) = FuturePure(a)

  implicit class FutureOpsCls[+A : Manifest](a: Exp[Future[A]]) extends FutureOpsBase[A] {
    def map[B: Manifest](f: Exp[A] => Exp[B]) = {
      val x = fresh[A]
      val b = reifyEffects(f(x))
      reflectEffect(FutureMap(a, x, b), summarizeEffects(b).star)
    }

    def flatMap[B: Manifest](f: Exp[A] => Exp[Future[B]]) = {
      val x = fresh[A]
      val b = reifyEffects(f(x))
      reflectEffect(FutureFlatMap(a, x, b), summarizeEffects(b).star)
    }

    def foreach(f: Exp[A] => Exp[Unit]) = {
      val x = fresh[A]
      val b = reifyEffects(f(x))
      reflectEffect(FutureForeach(a, x, b), summarizeEffects(b).star)
    }

    def withFilter(f: Exp[A] => Exp[Boolean]) = {
      val x = fresh[A]
      val b = reifyEffects(f(x))
      reflectEffect(FutureFilter(a, x, b), summarizeEffects(b).star)
    }
  }


  override def syms(e: Any) = e match {
    case FutureMap(a, _, b) => syms(a) ++ syms(b)
    case FutureFlatMap(a, _, b) => syms(a) ++ syms(b)
    case FutureForeach(a, _, b) => syms(a) ++ syms(b)
    case FutureFilter(a, _, b) => syms(a) ++ syms(b)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any) = e match {
    case FutureMap(_, x, b) => x +: effectSyms(b)
    case FutureFlatMap(_, x, b) => x +: effectSyms(b)
    case FutureForeach(_, x, b) => x +: effectSyms(b)
    case FutureFilter(_, x, b) => x +: effectSyms(b)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any) = e match {
    case FutureMap(a, _, b) => freqNormal(a) ++ freqNormal(b)
    case FutureFlatMap(a, _, b) => freqNormal(a) ++ freqNormal(b)
    case FutureForeach(a, _, b) => freqNormal(a) ++ freqNormal(b)
    case FutureFilter(a, _, b) => freqNormal(a) ++ freqNormal(b)
    case _ => super.symsFreq(e)
  }
}

trait JSGenFutureOps extends JSNestedCodegen {
  val IR: FutureOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case FuturePure(a) =>
      emitValDef(sym, "new Promise(" + quote(a) + ")")
    case FutureMap(a, x, b) =>
      emitValDef(sym, "new Promise()")
      stream.println(quote(a) + ".onComplete(function (" + quote(x) + ") {")
      emitBlock(b)
      stream.println(quote(sym) + ".complete(" + quote(getBlockResult(b)) + ");")
      stream.println("});")
    case FutureFlatMap(a, x, b) => // TODO optimize
      emitValDef(sym, "new Promise()")
      stream.println(quote(a) + ".onComplete(function (" + quote(x) + ") {")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)) + ".onComplete(function (v) {")
      stream.println(quote(sym) + ".complete(v);")
      stream.println("});")
      stream.println("});")
    case FutureForeach(a, x, b) =>
      stream.println("var " + quote(sym) + " = " + quote(a) + ".onComplete(function (" + quote(x) + ") {")
      emitBlock(b)
      stream.println("});")
    case FutureFilter(a, x, b) =>
      emitValDef(sym, "new Promise()")
      stream.println(quote(a) + ".onComplete(function (" + quote(x) + ") {")
      emitBlock(b)
      stream.println("if (" + quote(getBlockResult(b)) + " === true) {")
      stream.println(quote(sym) + ".complete(" + quote(getBlockResult(b)) + ");")
      stream.println("}")
      stream.println("});")
    case _ => super.emitNode(sym, rhs)
  }

  def emitDataStructures(out: PrintWriter) {
    // http://jsbin.com/ebecuk/5/edit
    out.println("""var Promise = function (value) {
                  |  if (value !== undefined) {
                  |    this.value = value;
                  |  }
                  |  this.callbacks = [];
                  |};
                  |Promise.prototype.complete = function (value) {
                  |  if (this.value !== undefined) {
                  |    throw "This promise has already been completed";
                  |  } else {
                  |    this.value = value;
                  |    for (var i = 0, n = this.callbacks.length ; i < n ; i++) {
                  |      var callback = this.callbacks[i];
                  |      callback(value);
                  |    }
                  |  }
                  |};
                  |Promise.prototype.onComplete = function (callback) {
                  |  if (this.value === undefined) {
                  |    this.callbacks.push(callback);
                  |  } else {
                  |    callback(this.value);
                  |  }
                  |};
                  |""".stripMargin)
    out.flush()
  }

}