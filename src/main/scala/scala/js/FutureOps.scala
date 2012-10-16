package scala.js

import virtualization.lms.common.{EffectExp, BaseExp, Base}
import concurrent.Future
import java.io.PrintWriter

trait FutureOps { this: Base =>

  implicit class FutureOpsCls[A : Manifest](a: Rep[Future[A]]) {
    def map[B : Manifest](f: Rep[A] => Rep[B]) = future_map(a, f)
    def flatMap[B : Manifest](f: Rep[A] => Rep[Future[B]]) = future_flatMap(a, f)
    def foreach(f: Rep[A] => Rep[Unit]) = future_foreach(a, f)
  }
  def future[A : Manifest](a: Rep[A]): Rep[Future[A]] // Wrong: should be a by-name parameter
  def future_map[A : Manifest, B : Manifest](a: Rep[Future[A]], f: Rep[A] => Rep[B]): Rep[Future[B]]
  def future_flatMap[A : Manifest, B : Manifest](a: Rep[Future[A]], f: Rep[A] => Rep[Future[B]]): Rep[Future[B]]
  def future_foreach[A : Manifest](a: Rep[Future[A]], f: Rep[A] => Rep[Unit]): Rep[Unit]
}

trait FutureOpsExp extends FutureOps with EffectExp {

  case class FuturePure[A](a: Exp[A]) extends Def[Future[A]]
  case class FutureMap[A, B](a: Exp[Future[A]], x: Sym[A], b: Block[B]) extends Def[Future[B]]
  case class FutureFlatMap[A, B](a: Exp[Future[A]], x: Sym[A], b: Block[Future[B]]) extends Def[Future[B]]
  case class FutureForeach[A](a: Exp[Future[A]], x: Sym[A], b: Block[Unit]) extends Def[Unit]

  def future[A : Manifest](a: Rep[A]) = FuturePure(a)

  def future_map[A : Manifest, B : Manifest](a: Exp[Future[A]], f: Exp[A] => Exp[B]) = {
    val x = fresh[A]
    val b = reifyEffects(f(x))
    reflectEffect(FutureMap(a, x, b), summarizeEffects(b).star)
  }

  def future_flatMap[A : Manifest, B : Manifest](a: Exp[Future[A]], f: Exp[A] => Exp[Future[B]]) = {
    val x = fresh[A]
    val b = reifyEffects(f(x))
    reflectEffect(FutureFlatMap(a, x, b), summarizeEffects(b).star)
  }

  def future_foreach[A : Manifest](a: Exp[Future[A]], f: Exp[A] => Exp[Unit]) = {
    val x = fresh[A]
    val b = reifyEffects(f(x))
    reflectEffect(FutureForeach(a, x, b), summarizeEffects(b).star)
  }

  override def syms(e: Any) = e match {
    case FutureMap(a, _, b) => syms(a) ++ syms(b)
    case FutureFlatMap(a, _, b) => syms(a) ++ syms(b)
    case FutureForeach(a, _, b) => syms(a) ++ syms(b)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any) = e match {
    case FutureMap(_, x, b) => x +: effectSyms(b)
    case FutureFlatMap(_, x, b) => x +: effectSyms(b)
    case FutureForeach(_, x, b) => x +: effectSyms(b)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any) = e match {
    case FutureMap(a, _, b) => freqNormal(a) ++ freqNormal(b)
    case FutureFlatMap(a, _, b) => freqNormal(a) ++ freqNormal(b)
    case FutureForeach(a, _, b) => freqNormal(a) ++ freqNormal(b)
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
      stream.println("var " + quote(sym) + " = " + quote(a) + ".map(function (" + quote(x) + ") {")
      emitBlock(b)
      stream.println("return " + quote(getBlockResult(b)))
      stream.println("});")
    case FutureFlatMap(a, x, b) =>
      stream.println("var " + quote(sym) + " = " + quote(a) + ".flatMap(function (" + quote(x) + ") {")
      emitBlock(b)
      stream.println("return " + quote(getBlockResult(b)))
      stream.println("});")
    case FutureForeach(a, x, b) =>
      stream.println("var " + quote(sym) + " = " + quote(a) + ".foreach(function (" + quote(x) + ") {")
      emitBlock(b)
      stream.println("});")
    case _ => super.emitNode(sym, rhs)
  }

  def emitDataStructures(out: PrintWriter) {
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
                  |Promise.prototype.foreach = function (callback) {
                  |  if (this.value === undefined) {
                  |    this.callbacks.push(callback);
                  |  } else {
                  |    callback(this.value);
                  |  }
                  |};
                  |Promise.prototype.map = function (f) {
                  |  var p = new Promise();
                  |  this.foreach(function (a) {
                  |    p.complete(f(a));
                  |  });
                  |  return p;
                  |};
                  |Promise.prototype.flatMap = function (f) {
                  |  var p = new Promise();
                  |  this.foreach(function (a) {
                  |    f(a).foreach(function (b) {
                  |      p.complete(b);
                  |    });
                  |  });
                  |  return p;
                  |};
                  |""".stripMargin)
    out.flush()
  }

}