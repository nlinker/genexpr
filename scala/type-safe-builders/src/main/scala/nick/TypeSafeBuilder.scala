package nick

object TypeSafeBuilder extends App {

  def result(args: Array[String]): AstBuilder = {
    implicit val builder = new AstBuilder()
    import Dsl._

    html {
      head {
        title(from → 1, to → 8) { +"Hello there!" }
      }
      body {
        a(href → "https://scala-lang.org") { +"link 1" }
        a(href → "https://scala-lang.org") { +"link 2" }
        +"123"
        p { +"hello" }
        +"234"
        p { +"hello, again" }
        +"345"
        args.foreach(arg ⇒ a(href → s"http://$arg.org") { +arg })
      }
    }
    builder
  }

  println(result(Array("hey", "hello", "there")))
}

object Dsl {

  import Boilerplate._

  val html = StandardNode("html")
  val head = StandardNode("head")
  val body = StandardNode("body")
  val title = TextOnlyNode("title")

  val a = LinkNode("a")
  val p = StandardNode("p")
  val t = TextNode

  val from = 'from
  val to = 'to
  val href = 'href

  implicit class TextWrapper(s: String) {
    def unary_+()(implicit xb: AstBuilder) = t(s)(xb)
  }

}

object Boilerplate {

  trait Attr {
    def prependTo(attributes: xml.MetaData): xml.MetaData
  }

  class AttrPair(val p: (Symbol, String)) extends Attr {
    def prependTo(attributes: xml.MetaData) =
      new xml.UnprefixedAttribute(p._1.name, p._2, attributes)
  }

  def attrsConv(args: Seq[(Symbol, Any)]) = {
    args
      .map { case (x, y) ⇒ new AttrPair(x, y.toString) }
      .foldRight(xml.Null: xml.MetaData) { case (a, as) => a.prependTo(as) }
  }

  /////////////
  trait TheNode
  /////////////

  case class StandardNode(tagName: String) extends TheNode {
    def apply(attrs: (Symbol, Any)*)(expr: ⇒ Any)(implicit builder: AstBuilder): Unit = {
      val e = new xml.Elem(null, tagName, attrsConv(attrs), xml.TopScope, false)
      builder.withElement(e, () ⇒ {
        expr // run the closure
      })
    }
    def apply(expr: ⇒ Any)(implicit builder: AstBuilder): Unit = {
      apply()(expr)(builder)
    }
  }

  // title, script, etc
  case class TextOnlyNode(tagName: String) extends TheNode {
    def apply(attrs: (Symbol, Any)*)(content: Any)(implicit builder: AstBuilder): Unit = {
      val children = content match {
        case _: Unit => Seq()
        case a       => Seq(new xml.Text(a.toString))
      }
      builder.addChild(new xml.Elem(null, tagName, attrsConv(attrs), xml.TopScope, false, children: _*))
    }
    def apply(expr: => Any)(implicit builder: AstBuilder): Unit = {
      apply()(expr)(builder)
    }
  }

  case class LinkNode(tagName: String) extends TheNode {
    def apply(attrs: (Symbol, Any)*)(content: Any)(implicit builder: AstBuilder): Unit = {
      val children = content match {
        case _: Unit => Seq()
        case a       => Seq(new xml.Text(a.toString))
      }
      builder.addChild(new xml.Elem(null, tagName, attrsConv(attrs), xml.TopScope, false, children: _*))
    }
  }

  case class SingleNode(tagName: String) extends TheNode {
    def apply(attrs: (Symbol, Any)*)(implicit builder: AstBuilder): Unit = {
      builder.addChild(new xml.Elem(null, tagName, attrsConv(attrs), xml.TopScope, true))
    }
  }

  object TextNode extends TheNode {
    def apply(text: String)(implicit builder: AstBuilder): Unit = {
      builder.addChild(new xml.Text(text))
    }
  }

}

trait WrapNode
case class WrapDoc(doc: xml.Document) extends WrapNode
case class WrapElem(elem: xml.Elem) extends WrapNode

class AstBuilder {

  // TODO build xml tree
  // val document = new xml.Document()
  // var current: WrapNode = WrapDoc(document)
  var ci = 0

  def addChild(node: xml.Node): Unit = {
    println(indent(ci) + "AddChild node: " + node)
  }

  def withElement(current: xml.Elem, run: () ⇒ Unit): Unit = {
    println(indent(ci) + "Before: " + current)
    ci += 1
    run() // recursive call
    ci -= 1
    println(indent(ci) + "After: " + current)
  }

  def indent(i: Int) = "  " * i
}
