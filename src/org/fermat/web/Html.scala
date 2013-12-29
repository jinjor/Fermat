package org.fermat.web

import scala.xml.Node
import org.fermat.Component

object Html {
  def apply(component: Component): HtmlComponent = {
    val template = apply(component.template.node)
    val script = component.script.node.map(node => node.text).getOrElse("")
    HtmlComponent(component, template, script)
  }
  def apply(node: Node): HtmlNode = {
    HtmlNode(node, node.child.map { child =>
      apply(child)
    })
  }

}

sealed abstract class Html
case class HtmlNode(node: Node, children: Seq[HtmlNode]) extends Html {
  def toHtmlString: String = {
    if (node.isAtom) node.text.trim else {
      //val attributes = node.attributes
      s"<${node.label}>${node.text.trim}</${node.label}>"//TODO
      //node.toString
    }
  }
}
case class HtmlComponent(component: Component, template: HtmlNode, script: String) extends Html {
  lazy val className = component.node.attribute("name").get.toString.capitalize
}