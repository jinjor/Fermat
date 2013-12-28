package org.fermat

import scala.xml.MetaData
import scala.xml.Node

object Event {
  def eventsOf(node: Node) = for {
    attr <- node.attributes
    if attr.key.startsWith("on")
    _eventName = attr.key.drop("on".length)
    eventName = _eventName.head.toLower + _eventName.tail
  } yield Event(attr, eventName)
  
}

case class Event(attribute: MetaData, name: String) {
  def methodName: String = (attribute.value).toString
}