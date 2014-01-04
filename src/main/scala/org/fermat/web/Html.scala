package org.fermat.web

import scala.xml.Node
import org.fermat.Component

object Html {
  def apply(component: Component, getComponentByTagLabel: String => Option[Component]): HtmlComponent = {
    val template = HtmlNode(component.template.node, HtmlInnerNodes(component.template.node.child.map { child =>
      apply(child, getComponentByTagLabel)
    }))
    val script = component.script.node.map(_.text).getOrElse("")
    HtmlComponent(component, template, script)
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
          if(node.toString.trim.isEmpty){
            
          }else{
            
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

  def toHtmlString(htmlNode: HtmlNode): String = {//TODO
    val node = htmlNode.node
    if (node.isAtom) "" else {
      //val attributes = node.attributes
      s"<${node.label}></${node.label}>" //TODO
      //node.toString
    }
  }
  def toHtmlString(htmlNode: HtmlTranscludeTargetNode): String = {
    val node = htmlNode.node
    if (node.isAtom) "" else {
      //val attributes = node.attributes
      s"<${node.label}></${node.label}>" //TODO
      //node.toString
    }
  }
  def toHtmlString(htmlNode: HtmlTranscludeArgNode): String = {
    val node = htmlNode.node
    if (node.isAtom) "" else {
      //val attributes = node.attributes
      s"<${node.label}></${node.label}>" //TODO
      //node.toString
    }
  }
  def classNameOf(component: Component): String = component.node.attribute("name").get.toString.capitalize

}
case class HtmlComponent(component: Component, template: HtmlNode, script: String)

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











