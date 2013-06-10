package scala.js.language.dom

import scala.js.language.React

trait DomReact extends React with Dom {

  case object events {
    def of(event: EventDef, target: Rep[EventTarget] = window, capture: Rep[Boolean] = unit(false))(implicit m: Manifest[event.Type]): Rep[Events[event.Type]] =
      Events[event.Type] { observer =>
        target.on(event, capture) { e => observer(e) }
      }

    def filtering(event: EventDef, target: Rep[EventTarget] = window, capture: Rep[Boolean] = unit(false))(p: Rep[event.Type] => Rep[Boolean])(implicit m: Manifest[event.Type]): Rep[Events[event.Type]] =
      of(event, target, capture).filter(p)
  }

}
