package scala.js

import org.scalatest.Suite
import virtualization.lms.common._
import concurrent.{Promise, Future}

class TestFutureOps extends FileDiffSuite2("test-out/") with Suite {

  trait Sleep { this: Base with FutureOps with Functions =>
    def sleep(delay: Rep[Int]): Rep[Future[Unit]] = {
      val p = promise[Unit]
      window_setTimeout(fun(p.put), delay)
      p.future
    }
    def window_setTimeout(f: Rep[Unit => Unit], d: Rep[Int]): Rep[Unit]
  }

  trait SleepExp extends Sleep { this: EffectExp with FutureOps with Functions =>
    case class WindowSetTimeout(f: Exp[Unit => Unit], d: Exp[Int]) extends Def[Unit]

    def window_setTimeout(f: Exp[Unit => Unit], d: Exp[Int]) = reflectEffect(WindowSetTimeout(f, d))
  }

  trait JSGenSleep extends JSNestedCodegen {
    val IR: EffectExp with SleepExp
    import IR._

    override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
      case WindowSetTimeout(f, d) =>
        stream.println("setTimeout(" + quote(f) + ", " + quote(d) + ");")
      case _ => super.emitNode(sym, rhs)
    }
  }

  trait Ajax { this: Base with FutureOps with Functions =>
    object Ajax {
      def get(url: Rep[String]): Rep[Future[String]] = {
        val p = promise[String]
        jQuery.get(url, fun(p.put))
        p.future
      }
    }

    val jQuery: jQueryOps
    trait jQueryOps {
      def get(url: Rep[String], f: Rep[String => Unit]): Rep[Unit]
    }
  }

  trait AjaxExp extends Ajax { this: EffectExp with FutureOps with Functions =>
    case class JQueryGet(url: Exp[String], f: Exp[String => Unit]) extends Def[Unit]
    object jQuery extends jQueryOps {
      def get(url: Exp[String], f: Exp[String => Unit]) = reflectEffect(JQueryGet(url, f))
    }
  }

  trait JSGenAjax extends JSNestedCodegen {
    val IR: EffectExp with AjaxExp
    import IR._

    override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
      case JQueryGet(url, f) =>
        stream.println("$.get(" + quote(url) + ", function (d, t, xhr) { " + quote(f) + "(xhr.responseText); });")
      case _ => super.emitNode(sym, rhs)
    }
  }

  def testForeach() {
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

    testWithOutFile("future-foreach") { out =>
      val prog = new Prog with EffectExp with SleepExp with FutureOpsExp with IfThenElseExp with TupledFunctionsRecursiveExp with JSDebugExp with LiftString with LiftNumeric
      val codegen = new JSGenEffect with JSGenSleep with JSGenFutureOps with JSGenIfThenElse with JSGenFunctions with JSGenDebug { val IR: prog.type = prog }
      codegen.emitSource0(prog.main _, "main", out)
    }
  }

  def testMapFlatMap() {
    trait Prog { this: Base with Ajax with FutureOps =>
      def main(url1: Rep[String]): Rep[Future[String]] = for {
        url2 <- Ajax.get(url1)
        text <- Ajax.get(url2)
      } yield text
    }

    testWithOutFile("future-mapflatmap") { out =>
      val prog = new Prog with EffectExp with AjaxExp with FutureOpsExp with IfThenElseExp with TupledFunctionsRecursiveExp
      val codegen = new JSGenEffect with JSGenAjax with JSGenFutureOps with JSGenIfThenElse with JSGenFunctions { val IR: prog.type = prog }
      codegen.emitSource(prog.main _, "main", out)
    }
  }

}
