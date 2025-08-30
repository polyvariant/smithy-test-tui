//> using dep "com.olvind.tui::tui:0.0.7"
//> using scala 3.7.3-RC2
//> using options -no-indent
import tui.*
import tui.widgets.*
import tui.crossterm.CrosstermJni
import tui.crossterm.KeyModifiers

@main def app: Unit = util.boundary[Unit] {
  withTerminal { (jni, terminal) =>
    while (true) {
      terminal.draw { f =>
        f.renderWidget(
          BlockWidget(
            title = Some(Spans(Array(Span("app", Style.DEFAULT)))),
            borders = Borders.ALL,
          ),
          terminal.viewport.area,
        )
      }

      jni.read() match {
        case key: tui.crossterm.Event.Key =>
          key.keyEvent.code match {
            case char: tui.crossterm.KeyCode.Char
                if char.c() == 'c' && (key.keyEvent().modifiers().bits() == KeyModifiers.CONTROL) =>
              util.boundary.break()
            case _ => ()
          }
        case _ => ()
      }
    }
  }
}
