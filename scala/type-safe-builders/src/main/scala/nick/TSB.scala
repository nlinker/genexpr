package nick

import scala.language.implicitConversions

object TSB extends App {

  def result(args: Array[String]): XmlBuilder = {
    implicit val xb = new XmlBuilder()
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
      }
    }
    xb
  }

  println(result(Array("hey", "hello", "there")).current)
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
    def unary_+()(implicit xb: XmlBuilder) = t(s)(xb)
  }

}

//implicit def attrBuilder1(s: Symbol) = new {
//  def ->(v: Any) = new AttrPair((s, v.toString))
//}
//
//implicit def attrBuilder2(s: Symbol) = new {
//  def →(v: Any) = new AttrPair((s, v.toString))
//}

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
    def apply(attrs: (Symbol, Any)*)(expr: ⇒ Any)(implicit builder: XmlBuilder): Unit = {
      builder.withElement(new xml.Elem(null, tagName, attrsConv(attrs), xml.TopScope, false), elem ⇒ {
        if (!builder.isDefined)
          builder.appendChild(elem)

        expr match {
          case _: Unit => ()
          case n: xml.Node => builder.appendChild(n)
          case a: Any => builder.appendChild(new xml.Text(a.toString))
        }
      })
    }

    def apply(expr: ⇒ Any)(implicit builder: XmlBuilder): Unit = {
      apply()(expr)(builder)
    }
  }

  // title, script, etc
  case class TextOnlyNode(tagName: String) extends TheNode {
    def apply(attrs: (Symbol, Any)*)(content: Any)(implicit builder: XmlBuilder): Unit = {
      val children = content match {
        case _: Unit => Seq()
        case a       => Seq(new xml.Text(a.toString))
      }
      builder.appendChild(new xml.Elem(null, tagName, attrsConv(attrs), xml.TopScope, false, children: _*))
    }

    def apply(expr: => Any)(implicit builder: XmlBuilder): Unit = {
      apply()(expr)(builder)
    }
  }

  case class LinkNode(tagName: String) extends TheNode {
    def apply(attrs: (Symbol, Any)*)(content: Any)(implicit builder: XmlBuilder): Unit = {
      val children = content match {
        case _: Unit => Seq()
        case a       => Seq(new xml.Text(a.toString))
      }
      builder.appendChild(new xml.Elem(null, tagName, attrsConv(attrs), xml.TopScope, false, children: _*))
    }
  }

  case class SingleNode(tagName: String) extends TheNode {
    def apply(attrs: (Symbol, Any)*)(implicit builder: XmlBuilder): Unit = {
      builder.appendChild(new xml.Elem(null, tagName, attrsConv(attrs), xml.TopScope, true))
    }
  }

  object TextNode extends TheNode {
    def apply(text: String)(implicit builder: XmlBuilder): Unit = {
      builder.appendChild(new xml.Text(text))
    }
  }

}

trait MyNode
case class MyDoc(doc: xml.Document) extends MyNode
case class MyElem(elem: xml.Elem) extends MyNode

class XmlBuilder {

  val document = new xml.Document()
  var current = Option.empty[xml.Elem]

  def isDefined = current.isDefined

  def appendChild(n: xml.Node): MyNode = {
    current match {
      case Some(elem) ⇒
        val e = elem.copy(child = elem.child ++ n)
        current = Some(e)
        MyElem(e)
      case None ⇒
        document.docElem = n
        current = Some(n.asInstanceOf[xml.Elem])
        MyDoc(document)
    }
  }

  def withElement(current: xml.Elem, run: xml.Elem ⇒ Unit): Unit = {
    run(current) // recursive call
  }
}

