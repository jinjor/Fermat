package org.fermat.web

import scala.xml.Node
import org.fermat.Component
import org.fermat.DefaultViewImpl
import org.fermat.Template
import org.fermat.NonLimitViewImpl
import org.fermat.Script

object Html {
  def apply(component: Component, getComponentByTagLabel: String => Option[Component]): HtmlComponent = {
    component.viewImpl match {
      case DefaultViewImpl(Template(templateNode), optScript) => {
        val template = HtmlNode(templateNode, HtmlInnerNodes(templateNode.child.map { child =>
          apply(child, getComponentByTagLabel)
        }))
        val script = optScript.map(_.node.text).getOrElse("")
        HtmlComponent(component, HtmlDefaultViewImpl(template, script))
      }
      case NonLimitViewImpl(Script(node)) => {
        val script = node.text
        HtmlComponent(component, HtmlNonLimitViewImpl(script))
      }
    }
  }
  
  
  def apply(node: Node, getComponentByName: String => Option[Component]): Html = {
    getComponentByName(node.label) match {
      case Some(component) => {
        val children = node.child.map { child =>
          toTranscludeArgNode(child, component, getComponentByName)
        }.flatten
        HtmlComponentNode(node, children, component)
      }
      case None => {
        if (node.label == "transclude") {
          HtmlTranscludeTargetNode(node)
        } else {
          val children = node.child.map { child =>
            apply(child, getComponentByName)
          }
          if (node.toString.trim.isEmpty) {

          } else {

          }
          val inner = (children match {
            case List(HtmlNode(node, _)) if node.isAtom => Some(node.text)
            case _ => None
          }) match {
            case Some(text) => HtmlInnerText(text)
            case None => HtmlInnerNodes(children)
          }
          HtmlNode(node, inner)
        }
      }
    }
  }

  def toTranscludeArgNode(node: Node, component: Component, getComponentByName: String => Option[Component]): Option[HtmlTranscludeArgNode] = {
    component.transcludeArgsAsMap.get(node.label) map {
      targ =>
        HtmlTranscludeArgNode(node, HtmlInnerNodes(node.child.map { child =>
          apply(child, getComponentByName)
        }))
    }
  }

  def toHtmlString(htmlNode: HtmlNode): String = { //TODO
    val node = htmlNode.node
    if (node.isAtom) "" else {
      val attributes = (node.attributes.map { attr =>
        s"""${attr.key}="${attr.value}""""
      }).mkString(" ")
      s"<${node.label} ${attributes}></${node.label}>"
    }
  }
  def toHtmlString(htmlNode: HtmlTranscludeTargetNode): String = {
    val node = htmlNode.node
    if (node.isAtom) "" else {
      val attributes = (node.attributes.map { attr =>
        s"""${attr.key}="${attr.value}""""
      }).mkString(" ")
      s"<${node.label} ${attributes}></${node.label}>"
    }
  }
  def toHtmlString(htmlNode: HtmlTranscludeArgNode): String = {
    val node = htmlNode.node
    if (node.isAtom) "" else {
      val attributes = (node.attributes.map { attr =>
        s"""${attr.key}="${attr.value}""""
      }).mkString(" ")
      s"<${node.label} ${attributes}></${node.label}>"
    }
  }
  def classNameOf(component: Component): String = component.node.attribute("name").get.toString.capitalize

}
case class HtmlComponent(component: Component, viewImpl: HtmlViewImpl)

sealed abstract class HtmlViewImpl
case class HtmlDefaultViewImpl(template: HtmlNode, script: String) extends HtmlViewImpl
case class HtmlNonLimitViewImpl(script: String) extends HtmlViewImpl

sealed abstract class HtmlInner
case class HtmlInnerText(text: String) extends HtmlInner
case class HtmlInnerNodes(value: Seq[Html]) extends HtmlInner

sealed abstract class Html
sealed abstract class HtmlWithInner extends Html {
  def node: Node
  def inner: HtmlInner
}
case class HtmlNode(node: Node, inner: HtmlInner) extends HtmlWithInner
case class HtmlTranscludeTargetNode(node: Node) extends Html
case class HtmlTranscludeArgNode(node: Node, inner: HtmlInner) extends HtmlWithInner
case class HtmlComponentNode(node: Node, children: Seq[HtmlTranscludeArgNode], component: Component) extends Html











