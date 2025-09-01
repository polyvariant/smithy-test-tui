import software.amazon.smithy.model.Model
import software.amazon.smithy.protocoltests.traits.HttpRequestTestsTrait
import scala.jdk.CollectionConverters.*
import software.amazon.smithy.model.node.Node
import software.amazon.smithy.protocoltests.traits.HttpResponseTestsTrait
import software.amazon.smithy.model.shapes.ShapeId
import scala.collection.immutable.ListMap
import software.amazon.smithy.protocoltests.traits.HttpMessageTestCase
import upickle.default.*
import software.amazon.smithy.model.validation.ValidationEvent
import software.amazon.smithy.model.node.NodeVisitor
import upickle.core.LinkedHashMap
import software.amazon.smithy.model.node.ObjectNode
import ujson.Value
import software.amazon.smithy.model.node.BooleanNode
import software.amazon.smithy.model.node.NullNode
import software.amazon.smithy.model.node.NumberNode
import software.amazon.smithy.model.node.StringNode

object finder {

  // Note: remember this function is cached
  def findTestTraits(m: Model): List[TestCase] = {
    m
      .getShapesWithTrait(classOf[HttpRequestTestsTrait])
      .asScala
      .toList
      .flatMap { s =>
        s.expectTrait(classOf[HttpRequestTestsTrait]).getTestCases().asScala.toList.map { t =>
          TestCase.make("request", s.getId, t)
        }
      } ++
      m
        .getShapesWithTrait(classOf[HttpResponseTestsTrait])
        .asScala
        .toList
        .flatMap { s =>
          s.expectTrait(classOf[HttpResponseTestsTrait]).getTestCases().asScala.toList.map { t =>
            TestCase.make("response", s.getId, t)
          }
        }
  }.sortBy(tc => (tc.protocol, tc.tpe, tc.id))

}

case class TestCase(
  tpe: String,
  shapeId: ShapeId,
  protocol: ShapeId,
  keys: Map[String, Node],
  id: String,
) derives ReadWriter

object TestCase {

  def make(tpe: "request" | "response", shapeId: ShapeId, node: HttpMessageTestCase): TestCase =
    TestCase(
      tpe,
      shapeId,
      node.getProtocol(),
      node
        .toNode()
        .expectObjectNode()
        .getMembers()
        .asScala
        .map((k, v) => (k.getValue(), v))
        .toMap - "protocol" - "id",
      node.getId(),
    )

}

given ReadWriter["request" | "response"] = readwriter[String].bimap(
  _.toString(),
  {
    case "request"  => "request"
    case "response" => "response"
  },
)

given ReadWriter[ShapeId] = readwriter[String].bimap(_.toString(), ShapeId.from)
given ReadWriter[Node] = readwriter[Value].bimap(fromNode, toNode)

def fromNode(node: Node): ujson.Value = node.accept(
  new NodeVisitor[ujson.Value] {

    def objectNode(node: ObjectNode): Value = ujson.Obj(
      LinkedHashMap(
        node
          .getMembers()
          .asScala
          .map((k, v) => (k.getValue(), v.accept(this)))
          .toSeq
      )
    )

    def arrayNode(node: software.amazon.smithy.model.node.ArrayNode): Value = ujson.Arr(
      node.getElements().asScala.map(_.accept(this)).toSeq
    )

    def booleanNode(node: BooleanNode): Value = ujson.Bool(node.getValue())

    def nullNode(node: NullNode): Value = ujson.Null
    def numberNode(node: NumberNode): Value = ujson.Num(node.getValue().doubleValue())
    def stringNode(node: StringNode): Value = ujson.Str(node.getValue())
  }
)

def toNode(value: ujson.Value): Node =
  value match {
    case ujson.Obj(m) =>
      Node.objectNode(
        m.map { case (k, v) => (Node.from(k), toNode(v)) }.toMap.asJava
      )
    case ujson.Arr(a)  => Node.arrayNode(a.map(toNode).toSeq*)
    case ujson.Str(s)  => Node.from(s)
    case ujson.Num(n)  => Node.from(n)
    case ujson.Bool(b) => Node.from(b)
    case ujson.Null    => Node.nullNode
  }
