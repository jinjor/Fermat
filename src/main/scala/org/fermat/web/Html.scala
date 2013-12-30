package org.fermat.web

import scala.xml.Node
import org.fermat.Component

object Html {
  def apply(component: Component, getComponentByTagLabel: String => Option[Component]): HtmlComponent = {
    val template = HtmlNode(component.template.node, component.template.node.child.map{ child =>
      apply(child, getComponentByTagLabel)
    })
    val script = component.script.node.map(_.text).getOrElse("")
    HtmlComponent(component, template, script)
  }
  def apply(node: Node, getComponentByName: String => Option[Component]): Html = {
    val children = node.child.map { child =>
      apply(child, getComponentByName)
    }
    getComponentByName(node.label) match {
      case Some(component) => HtmlComponentNode(node, children, component)
      case None => HtmlNode(node, children)
    }

  }
  def toHtmlString(htmlNode: HtmlNode): String = {
    val node = htmlNode.node
    if (node.isAtom) node.text.trim else {
      //val attributes = node.attributes
      s"<${node.label}>${node.text.trim}</${node.label}>" //TODO
      //node.toString
    }
  }
  def classNameOf(component: Component): String = component.node.attribute("name").get.toString.capitalize

}
case class HtmlComponent(component: Component, template: HtmlNode, script: String)

sealed abstract class Html
case class HtmlNode(node: Node, children: Seq[Html]) extends Html
case class HtmlComponentNode(node: Node, children: Seq[Html], component: Component) extends Html