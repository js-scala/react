package scala.js

import virtualization.lms.common._
import org.scalatest.Suite

class TestReact extends FileDiffSuite2("test-out/") with Suite {

  def testForeachAndMap() {

    trait Prog extends React with JSDom with NumericOps with JSDebug {

      def domEvents[A : Manifest](event: EventName[A], e: Rep[EventTarget] = window): Rep[Events[A]] = Events[A] { f =>
        e.on(event) { e => f(e) }
      }

      /*val move = Reactor.loop { self =>
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
      }*/

      def main(): Rep[Unit] = {
        val wheels = domEvents(MouseWheel)
        val deltaYs = wheels map (_.wheelDeltaY)
        for (deltaY <- deltaYs) log(deltaY)
      }
    }

    testWithOutFile("events-foreach") { out =>
      val prog = new Prog with EffectExp with React with JSDomExp with NumericOpsExp with JSFunctionsExp with IfThenElseExp with JSDebugExp
      val codegen = new JSGenEffect with JSGenDom with JSGenNumericOps with JSGenFunctions with JSGenIfThenElse with JSGenDebug { val IR: prog.type = prog }
      codegen.emitSource0(prog.main _, "main", out)
    }
  }
}
