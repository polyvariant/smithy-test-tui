//> using dep com.olvind.tui::tui:0.0.7
//> using dep com.softwaremill.ox::core:1.0.0
//> using dep software.amazon.smithy:smithy-model:1.61.0
//> using dep software.amazon.smithy:smithy-protocol-test-traits:1.61.0
//> using dep io.get-coursier:interface:1.0.28
//> using dep "com.lihaoyi::upickle:4.2.1"
//> using dep "com.lihaoyi::os-lib:0.11.5"
//> using scala 3.7.3-RC2
//> using options -no-indent

//> using publish.computeVersion git:tag
//> using publish.developers "kubukoz|Jakub Kozłowski|https://github.com/kubukoz"
//> using publish.license Apache-2.0
//> using publish.name smithy-test-tui
//> using publish.organization org.polyvariant
//> using publish.repository central
//> using publish.secretKey env:PGP_SECRET
//> using publish.secretKeyPassword env:PGP_PASSPHRASE
//> using publish.url https://github.com/polyvariant/smithy-test-tui
//> using publish.vcs github:polyvariant/smithy-test-tui
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
import software.amazon.smithy.model.Model
import scala.jdk.CollectionConverters.*
import software.amazon.smithy.model.validation.ValidationEvent
import upickle.default.*
import software.amazon.smithy.protocoltests.traits.HttpRequestTestsTrait
import software.amazon.smithy.model.node.Node
import software.amazon.smithy.model.node.StringNode
import software.amazon.smithy.model.node.ArrayNode
import software.amazon.smithy.model.node.ObjectNode
import software.amazon.smithy.model.shapes.ShapeId

case class MavenConfig(
  dependencies: List[String] = Nil
) derives ReadWriter

case class BuildConfig(
  maven: MavenConfig
) derives ReadWriter

enum Event {
  case Tick
  case Up
  case Down
  case PageUp
  case PageDown
  case Loaded(cases: List[TestCase])
  case Failed(errors: List[ValidationEvent])
}

enum State {
  case Loading
  case Loaded(items: List[TestCase], selected: Int)
  case Failed(errors: List[ValidationEvent])
}

object State {

  private val pageSize = 20

  extension (loaded: Loaded) {
    def next: Loaded = loaded.copy(selected = (loaded.selected + 1) % loaded.items.size)

    def prev: Loaded = loaded.copy(selected =
      (loaded.selected - 1 + loaded.items.size) % loaded.items.size
    )

    def pageUp: Loaded = loaded.copy(selected =
      (loaded.selected - pageSize + loaded.items.size) % loaded.items.size
    )

    def pageDown: Loaded = loaded.copy(selected = (loaded.selected + pageSize) % loaded.items.size)

  }

}

def renderLoaded(state: State.Loaded, f: Frame) = Layout(
  direction = Direction.Horizontal,
  constraints = Array(Constraint.Percentage(40), Constraint.Percentage(60)),
).split(f.size).tap { columns =>
  val item = state.items(state.selected)

  val protocols = state.items.view.map(_.protocol).toSet
  val protocolsUniqueByName =
    protocols.groupBy(_.getName).filter(_._2.size == 1).map(_._2.head).toSet

  def showProtocol(protocol: ShapeId) =
    if (protocolsUniqueByName.contains(protocol))
      protocol.getName
    else
      protocol.toString

  val protocolsWidth = protocols.view.map(showProtocol(_).length).max

  val casesTable = TableWidget(
    block = Some(
      BlockWidget(
        title = Some(
          Spans.nostyle(s"Available test cases (${state.selected + 1}/${state.items.size})")
        ),
        borders = Borders.ALL,
        borderStyle = Style.DEFAULT.fg(Color.Magenta),
      )
    ),
    widths = Array(
      Constraint.Min(protocolsWidth),
      Constraint.Min(List("request", "response").map(_.length).max),
      Constraint.Percentage(80),
    ),
    header = Some(
      TableWidget.Row(
        cells = Array(
          TableWidget.Cell(Text.nostyle("protocol")),
          TableWidget.Cell(Text.nostyle("kind")),
          TableWidget.Cell(Text.nostyle("id")),
        ).map(_.copy(style = Style.DEFAULT.fg(Color.Magenta).addModifier(Modifier.BOLD)))
      )
    ),
    rows =
      state
        .items
        .map { i =>
          TableWidget.Row(
            cells = Array(
              TableWidget.Cell(Text.nostyle(showProtocol(i.protocol))),
              TableWidget.Cell(Text.nostyle(i.tpe)),
              TableWidget.Cell(Text.nostyle(i.id)),
            )
          )
        }
        .toArray,
    highlightStyle = Style.DEFAULT.fg(Color.Magenta),
    highlightSymbol = Some("> "),
  )

  f.renderStatefulWidget(
    casesTable,
    columns(0),
  )(TableWidget.State(offset = 0, selected = Some(state.selected)))

  val maxKeyLen = item.keys.keys.map(_.length).maxOption.getOrElse(0) max "protocol".length

  val deetsKeyStyle = Style.DEFAULT.fg(Color.Green).addModifier(Modifier.BOLD)

  val deets = TableWidget(
    block = Some(
      BlockWidget(
        title = Some(Spans.nostyle("Test case details")),
        borders = Borders.ALL,
        borderStyle = Style.DEFAULT.fg(Color.Green).addModifier(Modifier.BOLD),
      )
    ),
    widths = Array(Constraint.Length(maxKeyLen), Constraint.Percentage(80)),
    rows = {
      List(
        TableWidget.Row(
          cells = Array(
            TableWidget.Cell(Text.nostyle("shapeId")),
            TableWidget.Cell(Text.nostyle(item.shapeId.toString())),
          )
        )
      ) ++
        item
          .keys
          .map { (k, v) =>
            TableWidget.Row(
              height = 1,
              cells = Array(
                TableWidget.Cell(Text.nostyle(k)),
                TableWidget.Cell(Text.nostyle {
                  v match {
                    case _: ObjectNode | _: ArrayNode => Node.printJson(v)
                    // flat nodes usually have better tostrings
                    case _ => v.toString
                  }
                }),
              ),
            )
          }
    }.map { row =>
      row.copy(cells = row.cells.zipWithIndex.map {
        case (cell, 0) => cell.copy(style = deetsKeyStyle)
        case (cell, _) => cell
      })
    }.toArray,
  )

  f.renderWidget(
    deets,
    columns(1),
  )

}

def update(
  state: State,
  event: Event,
): State =
  state match {
    case state: State.Loaded =>
      event match {
        case Event.Up       => state.prev
        case Event.Down     => state.next
        case Event.PageUp   => state.pageUp
        case Event.PageDown => state.pageDown
        case Event.Tick     => state
        case _              => state
      }
    case State.Loading =>
      event match {
        case Event.Loaded(cases)  => State.Loaded(cases, selected = 0)
        case Event.Failed(errors) => State.Failed(errors)
        case _                    => state
      }
    case State.Failed(_) => state
  }

def handleExit(
  event: tui.crossterm.Event
)(
  using Label[Unit]
) =
  event match {
    case key: tui.crossterm.Event.Key =>
      key.keyEvent.code match {
        case char: tui.crossterm.KeyCode.Char
            if char.c() == 'q' || (char
              .c() == 'c' && (key.keyEvent().modifiers().bits() == KeyModifiers.CONTROL)) =>
          boundary.break()
        case _ => ()
      }

    case _ => ()
  }

def handleLoaded(
  event: tui.crossterm.Event,
  channel: Channel[Event],
  selected: String,
): Unit =
  event match {
    case key: tui.crossterm.Event.Key =>
      key.keyEvent.code match {
        case char: tui.crossterm.KeyCode.Up       => channel.send(Event.Up)
        case char: tui.crossterm.KeyCode.Down     => channel.send(Event.Down)
        case keys: tui.crossterm.KeyCode.PageUp   => channel.send(Event.PageUp)
        case keys: tui.crossterm.KeyCode.PageDown => channel.send(Event.PageDown)
        case _                                    => channel.send(Event.Tick)
      }

    case _ => channel.send(Event.Tick)
  }

def log(s: Any) = os
  .write
  .append(
    os.pwd / "log.txt",
    s.toString + "\n",
  )

def loadBuild(build: BuildConfig): Either[List[ValidationEvent], List[TestCase]] = {

  val jars = ModelLoader.resolveDependencies(
    dependencies = build.maven.dependencies,
    repositories = Nil,
  )
  log(s"resolved dependencies: $jars")

  ModelLoader
    .load(
      specs = Set.empty,
      jars = jars,
    )
    .match {
      case r if r.isBroken() => Left(r.getValidationEvents().asScala.toList)
      case r                 =>
        val cases = finder.findTestTraits(r.unwrap())
        Right(cases)
    }
}

def cached[I: ReadWriter, E, O: ReadWriter](cacheFile: os.Path)(load: I => Either[E, O])
  : I => Either[E, O] =
  in => {
    def computeAndWrite = {
      val out = load(in)
      out.foreach { o =>
        os.write.over(cacheFile, write((in, o)), createFolders = true)
      }
      out
    }

    if (os.exists(cacheFile)) {
      val (previousIn, previousOut) = read[(I, O)](os.read(cacheFile))
      if (in == previousIn)
        Right(previousOut)
      else
        computeAndWrite
    } else
      computeAndWrite
  }

def timed[A](tag: String)(a: => A): A = {
  val (time, v) = ox.timed(a)
  log(
    s"${Console.CYAN}timed[${tag}] = ${time.toMillis} millis${Console.RESET}"
  )
  v
}

@main def app: Unit = {
  val channel = ox.channels.Channel.bufferedDefault[Event]

  var state: State = State.Loading

  util.boundary[Unit] {
    withTerminal { (jni, terminal) =>
      def renderLoop = ox.supervised {
        ox.forever {
          val e = channel.receive()
          // log("received event: " + e)

          val stateNew = update(state, e)

          // log("computed new state: " + stateNew)
          state = stateNew

          terminal.draw { f =>
            state match {
              case State.Loading =>
                ox.fork {
                  val build =
                    timed("read build")(read[BuildConfig](os.read(os.pwd / "smithy-build.json")))

                  val result = cached(os.pwd / ".smithy-testcase-viewer" / "cache.json")(loadBuild)
                    .apply(build)

                  val event = result.fold(
                    Event.Failed(_),
                    Event.Loaded(_),
                  )

                  channel.send(event)
                }

                f.renderWidget(
                  ParagraphWidget(
                    text = Text.nostyle("loading..."),
                    alignment = Alignment.Left,
                  ),
                  f.size,
                )
              case State.Failed(errors) =>
                val errorText = errors
                  .map(e => s"- [${e.getSeverity()}] ${e.getMessage()}")
                  .mkString("\n")

                f.renderWidget(
                  ParagraphWidget(
                    text = Text.nostyle(
                      s"failed to load model with the following errors:\n\n${errorText}Press q to exit and try again."
                    ),
                    alignment = Alignment.Left,
                  ),
                  f.size,
                )
              case state: State.Loaded => timed("renderLoaded")(renderLoaded(state, f))
            }
          }
        }
      }

      def eventLoop = {
        channel.send(Event.Tick)

        ox.forever {
          val e = jni.read()
          // log("got event: " + e)
          handleExit(e)

          state match {
            case state: State.Loaded => handleLoaded(e, channel, state.items(state.selected).id)
            case _                   => ()
          }
        }
      }

      ox.par(renderLoop, eventLoop)._2

    }
  }
}
