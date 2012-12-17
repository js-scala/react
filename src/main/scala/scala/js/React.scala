package scala.js

import virtualization.lms.common.{Structs, Functions, Base}

trait React { this: Base with Functions =>

  type RepEvents[+A] = Rep[(A => Unit) => Unit]

  def Events[A : Manifest](es: Rep[(A => Unit)] => Rep[Unit]): RepEvents[A] = es

  implicit class EventsOps[A : Manifest](es: RepEvents[A]) {

    def foreach(f: Rep[A] => Rep[Unit]): Rep[Unit] = es(f)

    def map[B : Manifest](f: Rep[A] => Rep[B]): RepEvents[B] = Events[B] { g =>
      es((e: Rep[A]) => g(f(e)))
    }

    def merge[B >: A : Manifest](bs: RepEvents[B]): RepEvents[B] = Events[B] { f =>
      es((a: Rep[A]) => f(a))
      bs((b: Rep[B]) => f(b))
    }
  }

  /*trait EventSource[A] extends Events[A]
  implicit class EventSourceOps[A](es: Rep[EventSource[A]]) {
    def emit(a: Rep[A]): Rep[Unit] = eventsource_emit(es, a)
  }
  def eventsource_emit[A](es: Rep[EventSource[A]], a: Rep[A]): Rep[Unit]
  object EventSource {
    def apply[A](f: Rep[EventSource[A]] => Rep[Unit]) = eventsource_apply[A](f)
  }
  def eventsource_apply[A](f: Rep[EventSource[A]] => Rep[Unit]): Rep[EventSource[A]]*/

  /*trait Observer[A]
  implicit class ObserverOps[A](o: Rep[Observer[A]]) {
    def dispose(): Rep[Unit] = observer_dispose(o)
  }
  def observer_dispose[A](o: Rep[Observer[A]]): Rep[Unit]

  trait Reactor
  implicit class ReactorOps(r: Rep[Reactor]) {
    def await[A](e: Rep[Events[A]]) = reactor_await(r, e)
    def awaitNext[A](e: Rep[Events[A]]) = reactor_awaitNext(r, e)
    def loopUntil[A](e: Rep[Events[A]])(handler: => Rep[Unit]) = reactor_loopUntil(r, e, handler)
  }
  def reactor_await[A](r: Rep[Reactor], e: Rep[Events[A]]): Rep[Future[A]]
  def reactor_awaitNext[A](r: Rep[Reactor], e: Rep[Events[A]]): Rep[Future[A]]
  def reactor_loopUntil[A](r: Rep[Reactor], e: Rep[Events[A]], handler: => Rep[Unit]): Rep[Future[A]]

  def observe[A](es: Rep[Events[A]])(handler: Rep[A] => Unit): Rep[Observer[A]]
  object Reactor {
    def loop(f: Rep[Reactor] => Rep[Unit]) = reactor_loop(f)
  }
  def reactor_loop(f: Rep[Reactor] => Rep[Unit]): Rep[Unit]

  trait Signal[+A]
  implicit class SignalOps[A](s: Rep[Signal[A]]) {
    def apply() = signal_apply(s)
    def changes = signal_changes(s)
  }
  def signal_apply[A](s: Rep[Signal[A]]): Rep[A]
  def signal_changes[A](s: Rep[Signal[A]]): Rep[Events[A]]
  object Signal {
    def apply[A](b: => Rep[A]) = signal_factory(b)
  }
  def signal_factory[A](b: => Rep[A]): Rep[Signal[A]]

  trait SVar[A] extends Signal[A]
  implicit class SVarOps[A](s: Rep[SVar[A]]) {
    def update(value: Rep[A]) = svar_update(s, value)
  }
  def svar_update[A](s: Rep[SVar[A]], value: Rep[A]): Rep[A]*/
}
