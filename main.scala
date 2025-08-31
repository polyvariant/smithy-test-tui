//> using dep "com.olvind.tui::tui:0.0.7"
//> using dep "com.softwaremill.ox::core:1.0.0"
//> using scala 3.7.3-RC2
//> using options -no-indent
import tui.*
import tui.widgets.*
import tui.crossterm.CrosstermJni
import tui.crossterm.KeyModifiers
import java.util.UUID
import tui.widgets.ParagraphWidget.Wrap
import util.chaining.*
import java.time.Instant
import scala.concurrent.duration.*
import scala.util.boundary.Label
import ox.channels.Channel
import scala.util.boundary
import java.nio.file.Files
import java.nio.file.Paths

enum Event {
  case Tick
  case Up
  case Down
  case New(id: String)
}

enum State {
  case Loading
  case Loaded(selected: Int, items: List[String])
}

object State {

  extension (loaded: Loaded) {
    def next: Loaded = loaded.copy(selected = (loaded.selected + 1) % loaded.items.size)

    def prev: Loaded = loaded.copy(selected =
      (loaded.selected - 1 + loaded.items.size) % loaded.items.size
    )

    def addItem(item: String): Loaded = loaded.copy(items = loaded.items :+ item)

  }

}

def renderLoaded(state: State.Loaded, f: Frame) = Layout(
  direction = Direction.Horizontal,
  constraints = Array(Constraint.Percentage(30), Constraint.Percentage(50)),
).split(f.size).tap { columns =>
  val item = state.items(state.selected)

  val list = ListWidget(
    block = Some(
      BlockWidget(
        title = Some(
          Spans.nostyle("select your poison")
        ),
        borders = Borders.ALL,
      )
    ),
    items =
      state
        .items
        .map { i =>
          ListWidget.Item(
            content = Text.nostyle(i)
          )
        }
        .toArray,
    style = Style.DEFAULT,
    startCorner = Corner.TopLeft,
    highlightStyle = Style.DEFAULT.fg(Color.Red),
    highlightSymbol = Some("> "),
  )

  f.renderStatefulWidget(
    list,
    columns(0),
  )(ListWidget.State(offset = 0, selected = Some(state.selected)))

  Layout(
    direction = Direction.Vertical,
    constraints = Array(Constraint.Length(1), Constraint.Min(1)),
    margin = Margin(1),
  ).split(columns(1)).pipe { rhsRows =>
    f.renderWidget(
      BlockWidget(
        title = Some(Spans.nostyle("details")),
        borders = Borders.ALL,
        borderStyle = Style.DEFAULT.fg(Color.Green),
      ),
      columns(1),
    )

    val header = ParagraphWidget(
      text = Text.nostyle(s"see all the details about ${item} here")
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

def update(
  state: State.Loaded,
  event: Event,
)(
  using Label[Option[String]]
): State.Loaded =
  event match {
    case Event.Up      => state.prev
    case Event.Down    => state.next
    case Event.New(id) => state.addItem(id)
    case Event.Tick    => state
  }

def handleLoaded(
  event: tui.crossterm.Event,
  channel: Channel[Event],
  selected: String,
)(
  using Label[Option[String]]
): Unit =
  event match {
    case key: tui.crossterm.Event.Key =>
      key.keyEvent.code match {
        case char: tui.crossterm.KeyCode.Char
            if char.c() == 'q' || (char
              .c() == 'c' && (key.keyEvent().modifiers().bits() == KeyModifiers.CONTROL)) =>
          boundary.break(None)
        case char: tui.crossterm.KeyCode.Char if char.c() == 'n' =>
          channel.send(Event.New(s"NEW ITEM! ${UUID.randomUUID()}"))
        case char: tui.crossterm.KeyCode.Up    => channel.send(Event.Up)
        case char: tui.crossterm.KeyCode.Down  => channel.send(Event.Down)
        case keys: tui.crossterm.KeyCode.Enter => boundary.break(Some(selected))
        case _                                 => channel.send(Event.Tick)
      }

    case _ => channel.send(Event.Tick)
  }

def log(s: Any) = Files.writeString(
  Paths.get("log.txt"),
  s.toString + "\n",
  java.nio.file.StandardOpenOption.CREATE,
  java.nio.file.StandardOpenOption.APPEND,
)

@main def app: Unit = {
  val channel = ox.channels.Channel.bufferedDefault[Event]

  var state = new State.Loaded(0, List("beer", "taxes"))

  val choice = util.boundary[Option[String]] {
    withTerminal { (jni, terminal) =>
      def renderLoop = ox.forever {
        val e = channel.receive()
        // log("received event: " + e)

        val stateNew = update(state, e)

        // log("computed new state: " + stateNew)
        state = stateNew

        terminal.draw { f =>
          renderLoaded(state, f)
        }
      }

      def eventLoop = {
        channel.send(Event.Tick)

        ox.forever {
          val e = jni.read()
          // log("got event: " + e)
          handleLoaded(e, channel, state.items(state.selected))
        }
      }

      ox.par(renderLoop, eventLoop)._2

      None
    }
  }

  choice.foreach { c =>
    println(s"congrats, you've chosen ${c}!")
  }
}
