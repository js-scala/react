package scala.js

import virtualization.lms.common.{NumericOps, Base}

class TestReact {

  trait Prog { this: Base with FutureOps with React with JSDom with NumericOps =>

    def events[A : Manifest](event: EventName[A], e: Rep[EventTarget] = window): Rep[Events[A]] = EventSource[A] { self =>
      e.on(event) { self.emit(_) }
    }

    val move = Reactor.loop { self =>
      for {
        down <- self await events(MouseDown)
        _ <- self.loopUntil(events(MouseUp)) {
          def move(prevX: Rep[Double], prevY: Rep[Double]): Rep[Unit] =
            for (e <- self awaitNext events(MouseMove)) {
              val x = prevX + e.offsetX
              val y = prevY + e.offsetY
              // TODO Here update the user interface
              move(x, y)
            }
          move(down.offsetY, down.offsetY)
        }
      } ()
    }

  }

}
