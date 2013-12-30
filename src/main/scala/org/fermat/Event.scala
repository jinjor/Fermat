package org.fermat

import scala.xml.MetaData
import scala.xml.Node

object Event {
  def eventsOf(node: Node): Iterable[Event] = for {
    attr <- node.attributes
    if attr.key.startsWith("on")
    _eventName = attr.key.drop("on".length)
    eventName = _eventName.head.toLower + _eventName.tail
  } yield Event(attr, eventName)
  
  def apply(attribute: MetaData, name: String): Event = {
    Event(attribute.value.toString, name)
  }
}
case class Event(methodName:String, name: String)