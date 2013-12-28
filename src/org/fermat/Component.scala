package org.fermat

import scala.xml.Elem
import scala.xml.Node
import scala.xml.Attribute
import scala.xml.MetaData

case class Require(node: Node)
case class Template(node: Node)
case class Script(node: Option[Node])

object Component {
  def apply(node: Node): Component = {
    val requires = (node \ "require").map(xml => Require(xml))
    val template = (node \ "template")(0)
    val script = (node \ "script").headOption
    Component(node, requires, Template(template), Script(script))
  }
  case class Event(private val attribute: MetaData, name: String) {
	 def methodName: String = (attribute.value).toString
  }
  
}
case class Component(node: Node, requires:Seq[Require], template: Template, script: Script)












