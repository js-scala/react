package scala.js.language

import scala.virtualization.lms.common.{IfThenElse, Base}
import scala.concurrent.{Future, Promise}

// TODO Encode at the type level whether a promise has been completed or not
// TODO Reify Future operations
trait FutureOps extends Base with IfThenElse {

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
