package scala.js.gen.js.dom

import scala.js.gen.js.GenReact
import scala.js.exp.dom.DomReactExp

trait GenDomReact extends GenReact with GenDom {
  val IR: DomReactExp
}