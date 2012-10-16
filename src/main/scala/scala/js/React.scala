package scala.js

import virtualization.lms.common.Base
import concurrent.Future

trait React { this: Base with JSDom =>

  /*trait Events[+A]
  implicit class EventsOps[A](e: Rep[Events[A]]) {
    def merge[B >: A](e: Rep[Events[B]]): Rep[Events[B]]
    def map[B](f: Rep[A] => Rep[B]): Rep[Events[B]]
  }
  implicit def eventsToEventsOps[A](e: Rep[Events[A]]) = new EventsOps[A](e)

  trait EventSource[A] extends Events[A]
  implicit class EventSourceOps[A](es: Rep[EventSource[A]]) {
    def emit(a: A): Rep[Unit]
  }

  trait Observer[A]
  implicit class ObserverOps[A](o: Rep[Observer[A]]) {
    def dispose(): Rep[Unit]
  }

  trait Reactor
  implicit class ReactorOps(r: Rep[Reactor]) {
    def await[A](e: Rep[Events[A]]): Rep[Future[A]]
    def awaitNext[A](e: Rep[Events[A]]): Rep[Future[A]]
    def loopUntil[A](e: Rep[Events[A]])(handler: => Rep[Unit]): Rep[Future[A]]
  }

  def observe[A](es: Rep[Event[A]])(handler: Rep[A] => Unit): Rep[Observer[A]]
  object Reactor {
    def loop(f: Rep[Reactor] => Rep[Unit]): Rep[Unit]
  }*/

}
