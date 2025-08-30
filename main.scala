//> using dep "com.olvind.tui::tui:0.0.7"
//> using scala 3.7.3-RC2
//> using options -no-indent
import tui.*
import tui.widgets.*
import tui.crossterm.CrosstermJni
import tui.crossterm.KeyModifiers
import java.util.UUID

@main def app: Unit = {
  var selected = 0
  extension (i: Int) {
    def clamped(range: Range): Int =
      if (i < range.start)
        range.end - 1
      else if (i >= range.end)
        range.start
      else
        i
  }
  var items = List(
    "beer",
    "taxes",
  )

  val choice = util.boundary[Option[String]] {
    withTerminal { (jni, terminal) =>
      while (true) {
        terminal.draw { f =>
          val block = BlockWidget(
            title = Some(Spans(Array(Span("select your poison", Style.DEFAULT)))),
            borders = Borders.ALL,
          )

          val list = ListWidget(
            block = Some(block),
            items =
              items
                .map(i =>
                  ListWidget.Item(
                    content = Text(
                      Array(
                        Spans(
                          Array(Span(i, Style.DEFAULT))
                        )
                      )
                    )
                  )
                )
                .toArray,
            style = Style.DEFAULT,
            startCorner = Corner.TopLeft,
            highlightStyle = Style.DEFAULT.fg(Color.Red),
            highlightSymbol = Some("> "),
          )

          f.renderStatefulWidget(
            list,
            terminal.viewport.area,
          )(ListWidget.State(offset = 0, selected = Some(selected)))
        }

        jni.read() match {
          case key: tui.crossterm.Event.Key =>
            key.keyEvent.code match {
              case char: tui.crossterm.KeyCode.Char
                  if char.c() == 'q' || (char
                    .c() == 'c' && (key.keyEvent().modifiers().bits() == KeyModifiers.CONTROL)) =>
                util.boundary.break(None)
              case char: tui.crossterm.KeyCode.Char if char.c() == 'n' =>
                items :+= s"NEW ITEM! ${UUID.randomUUID()}"
              case char: tui.crossterm.KeyCode.Up =>
                selected = (selected - 1).clamped(items.indices)
              case char: tui.crossterm.KeyCode.Down =>
                selected = (selected + 1).clamped(items.indices)
              case keys: tui.crossterm.KeyCode.Enter => util.boundary.break(Some(items(selected)))
              case _                                 => ()
            }
          case _ => ()
        }
      }
    }

    None
  }

  choice.foreach { c =>
    println(s"congrats, you've chosen ${c}!")
  }
}
