package org.fermat

import scala.xml.Node
import scala.xml.Text
import scala.xml.MetaData

object FermatNode {
  def apply(node: Node, isComponent: String => Boolean): FermatNode = {
    val label = node.label
    val attributes = attributesOf(node)
    def createNode = if (isComponent(label)) { (label: String, attributes: Iterable[FermatAttribute], _children: Seq[FermatNode]) =>
      val children = _children.flatMap {
        _ match {
          case n: FermatGeneralNodeLike => Some(n)
          case _ => None
        }
      }
      FermatComponentNode(label, attributes, children)
    } else FermatGeneralNode.apply _

    if (node.isAtom) {
      if (node.isInstanceOf[Text]) {
        FermatTextNode(FermatText(node.toString))
      } else {
        createNode(label, attributes, Seq())
      }
    } else {
      val multi = node.child.length > 1
      val children = node.child.flatMap { c =>
        if (multi && c.isInstanceOf[Text]) {
          None
        } else {
          Some(FermatNode(c, isComponent))
        }
      }
      createNode(label, attributes, children)
    }
  }

  def attributesOf(node: Node): Iterable[FermatAttribute] = for {
    attr: MetaData <- node.attributes
    key = attr.key
    value = attr.value.toString
  } yield {
    if (key.startsWith("on")) {
      val _eventName = key.drop("on".length)
      val eventName = _eventName.head.toLower + _eventName.tail
      FermatEventAttribute(Event(value, eventName))
    } else {
      FermatGeneralAttribute(key, FermatText(value))
    }
  }
}

case class Event(methodName: String, name: String)

abstract sealed class FermatNode

private object FermatText {
  def apply(s: String): FermatText = {
    val modelNames = TemplateParser.extractParamsFromString(s)
    if (modelNames.isEmpty) {
      FermatStaticText(s)
    } else {
      FermatDynamicText(modelNames)
    }
  }
}
abstract sealed class FermatText
case class FermatStaticText(value: String) extends FermatText
case class FermatDynamicText(modelNames: Set[String]) extends FermatText

abstract sealed class FermatAttribute
case class FermatGeneralAttribute(key: String, value: FermatText) extends FermatAttribute
case class FermatEventAttribute(event: Event) extends FermatAttribute

trait FermatGeneralNodeLike extends FermatNode {
  def label: String
  def attributes: Iterable[FermatAttribute]
  def children: Seq[FermatNode]
  if (label == "template") {
    throw new IllegalArgumentException()
  }

  private lazy val attributesAsMap = {
    val pairs = attributes.flatMap {
      _ match {
        case attr: FermatGeneralAttribute => Some(attr.key, attr)
        case _ => None
      }
    }
    pairs.toMap
  }
  def attribute(attrName: String): Option[FermatGeneralAttribute] = {
    attributesAsMap.get(attrName)
  }
}
case class FermatComponentNode(label: String, attributes: Iterable[FermatAttribute], children: Seq[FermatGeneralNodeLike]) extends FermatGeneralNodeLike

case class FermatGeneralNode(label: String, attributes: Iterable[FermatAttribute], children: Seq[FermatNode]) extends FermatGeneralNodeLike {
  private lazy val childrenAsMap = {
    val pairs = (children.flatMap { c =>
      c match {
        case node: FermatGeneralNodeLike => Some(node)
        case _ => None
      }
    }).groupBy(_.label)
    pairs.toMap
  }
  lazy val events = (attributes.flatMap { a =>
    a match {
      case a: FermatEventAttribute => Some(a)
      case _ => None
    }
  })
  def \(childName: String): Seq[FermatNode] = {
    childrenAsMap.get(childName).getOrElse(Seq.empty)
  }
}
case class FermatTextNode(text: FermatText) extends FermatNode
