package scala.js

import virtualization.lms.common.Base

class TestReact {

  /*trait Prog { this: Base with FutureOps with React with JSDom =>

    def events[A](event: Rep[EventDef[A]], e: Rep[Element]): Events[A] = EventSource[A] {
      e.on(event) { emit(_) }
    }

    val move = Reactor.loop { self =>
      for {
        down <- self await events(MouseDown, window)
        _ <- self.loopUntil(events(MouseUp, window)) {
          def move(origX: Rep[Double], origY: Rep[Double], prev: Rep[MouseEvent]): Rep[Unit] =
            for (e <- self awaitNext events(MouseMove, window)) {
              val x = Math.round(orig.x + (e.offsetX - prev.x) * 100.0 / scale)
              val y = Math.round(orig.y + (e.offsetY - prev.y) * 100.0 / scale)
              updateTransform(s, x, y)
              move(x, y, e)
            }
          move(tx, ty, down)
        }
      } yield ()
    }

  }*/
}
