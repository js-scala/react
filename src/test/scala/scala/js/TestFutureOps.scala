package scala.js

import org.scalatest.Suite
import virtualization.lms.common._
import concurrent.{Promise, Future}

class TestFutureOps extends FileDiffSuite2("test-out/") with Suite {

  trait Sleep { this: Base with FutureOps =>
    def sleep(delay: Rep[Int]): Rep[Future[Unit]]
  }

  trait SleepExp extends Sleep { this: EffectExp with FutureOps =>
    case class Sleep(delay: Exp[Int]) extends Def[Future[Unit]]
    def sleep(delay: Exp[Int]) = reflectEffect(Sleep(delay))
  }

  trait JSGenSleep extends JSNestedCodegen {
    val IR: EffectExp with SleepExp
    import IR._

    override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
      case Sleep(delay) =>
        emitValDef(sym, "new Promise()")
        stream.println("setTimeout(function () { " + quote(sym) + ".complete(null); }, " + quote(delay) + ")")
      case _ => super.emitNode(sym, rhs)
    }
  }

  trait Ajax { this: Base with FutureOps =>
    val Ajax: AjaxOps

    trait AjaxOps {
      def get(url: Rep[String]): Rep[Future[String]]
    }
  }

  trait AjaxExp extends Ajax { this: EffectExp with FutureOps =>
    object Ajax extends AjaxOps {
      def get(url: Exp[String]) = reflectEffect(AjaxGet(url))
    }

    case class AjaxGet(url: Rep[String]) extends Def[Future[String]]
  }

  trait JSGenAjax extends JSNestedCodegen {
    val IR: EffectExp with AjaxExp
    import IR._

    override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
      case AjaxGet(url) =>
        emitValDef(sym, "new Promise()")
        stream.println("$.get(" + quote(url) + ", function (d, t, xhr) { " + quote(sym) + ".complete(xhr.responseText) });")
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
      val prog = new Prog with EffectExp with SleepExp with FutureOpsExp with JSDebugExp with LiftString with LiftNumeric
      val codegen = new JSGenEffect with JSGenSleep with JSGenFutureOps with JSGenDebug { val IR: prog.type = prog }
      codegen.emitDataStructures(out)
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
      val prog = new Prog with EffectExp with AjaxExp with FutureOpsExp
      val codegen = new JSGenEffect with JSGenAjax with JSGenFutureOps { val IR: prog.type = prog }
      codegen.emitDataStructures(out)
      codegen.emitSource(prog.main _, "main", out)
    }
  }

}
