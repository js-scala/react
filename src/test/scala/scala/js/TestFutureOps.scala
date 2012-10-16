package scala.js

import org.scalatest.Suite
import virtualization.lms.common._
import concurrent.{Promise, Future}

class TestFutureOps extends FileDiffSuite2("test-out/") with Suite {

  trait Sleep { this: Base with FutureOps =>
    def sleep(delay: Rep[Int]): Rep[Future[Unit]]
  }

  trait SleepExp extends Sleep { this: EffectExp with FutureOps =>
    /*case class SleepBuilder(build: (Exp[Unit] => Exp[Unit]) => Unit) extends Exp[Future[Unit]]
    case class Sleep(delay: Exp[Int], block: => Block[Unit]) extends Def[Unit]
    def sleep(delay: Exp[Int]) = {
      SleepBuilder { callback =>
        val x = fresh[Unit]
        val b = reifyEffects(callback(x))
        Sleep(delay, b)
      }
    }*/
    case class Sleep(delay: Exp[Int]) extends Def[Future[Unit]]
    def sleep(delay: Exp[Int]) = reflectEffect(Sleep(delay))
  }

  trait JSGenSleep extends JSNestedCodegen {
    val IR: EffectExp with SleepExp
    import IR._

    override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
      /*case Sleep(delay, block) =>
        stream.println("window.setTimeout(function () {")
        emitBlock(block)
        stream.println("}, " + quote(delay) + ");")*/
      case Sleep(delay) =>
        emitValDef(sym, "new Promise()")
        stream.println("setTimeout(function () { " + quote(sym) + ".complete(null); }, " + quote(delay) + ")")
      case _ => super.emitNode(sym, rhs)
    }
  }

  trait Prog { this: Base with Sleep with FutureOps with JSDebug with LiftString with LiftNumeric =>

    /*
      var main = function () {
        setTimeout(function () {
          console.log("Hello");
          setTimeout(function () {
            console.log(", World!");
          }, 2000)
        }, 1000);
      };
     */
    def main(): Rep[Unit] = {
      for {
        _ <- sleep(1000)
        _ <- future(log("Hello"))
        _ <- sleep(2000)
        _ <- future(log(", World!"))
      } ()
    }
  }

  def testFutureOps() {
    testWithOutFile("future") { out =>
      val prog = new Prog with EffectExp with SleepExp with FutureOpsExp with JSDebugExp with LiftString with LiftNumeric with TupleOpsExp
      val codegen = new JSGenEffect with JSGenSleep with JSGenFutureOps with JSGenDebug { val IR: prog.type = prog }
      codegen.emitDataStructures(out)
      codegen.emitSource0(prog.main _, "future", out)
    }
  }

}
