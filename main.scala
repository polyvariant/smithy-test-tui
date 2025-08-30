//> using dep "com.olvind.tui::tui:0.0.7"
//> using scala 3.7.3-RC2
//> using options -no-indent
import tui.*
import tui.widgets.*
import tui.crossterm.CrosstermJni
import tui.crossterm.KeyModifiers
import java.util.UUID
import tui.widgets.ParagraphWidget.Wrap
import util.chaining.*

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
        val item = items(selected)

        terminal.draw { f =>
          Layout(
            direction = Direction.Horizontal,
            constraints = Array(Constraint.Percentage(30), Constraint.Percentage(50)),
          ).split(f.size).tap { columns =>
            val list = ListWidget(
              block = Some(
                BlockWidget(
                  title = Some(Spans(Array(Span("select your poison", Style.DEFAULT)))),
                  borders = Borders.ALL,
                )
              ),
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
              columns(0),
            )(ListWidget.State(offset = 0, selected = Some(selected)))

            Layout(
              direction = Direction.Vertical,
              constraints = Array(Constraint.Length(1), Constraint.Min(1)),
              margin = Margin(1),
            ).split(columns(1)).pipe { rhsRows =>
              f.renderWidget(
                BlockWidget(
                  title = Some(Spans(Array(Span("details", Style.DEFAULT)))),
                  borders = Borders.ALL,
                  borderStyle = Style.DEFAULT.fg(Color.Green),
                ),
                columns(1),
              )

              val header = ParagraphWidget(
                text = Text(
                  Array(
                    Spans(Array(Span(s"see all the details about ${item} here", Style.DEFAULT)))
                  )
                )
              )

              val deets = ParagraphWidget(
                text = Text.nostyle(
                  s"oh yes this is the details! Let me tell you the full story of lorem ipsum, friends, adventure and betrayal.\n\nmaybe even some more things too."
                ),
                wrap = Some(Wrap(trim = false)),
              )

              f.renderWidget(header, rhsRows(0))

              f.renderWidget(
                deets,
                rhsRows(1).inner(Margin(1, 0)),
              )

            }
          }
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
