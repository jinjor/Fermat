package org.fermat.web

import scala.xml.Node
import org.fermat.Component
import org.fermat.DefaultViewImpl
import org.fermat.Template
import org.fermat.NonLimitViewImpl
import org.fermat.Script
import org.fermat.FermatNode
import org.fermat.FermatGeneralNodeLike
import org.fermat.FermatComponentNode
import org.fermat.FermatGeneralNode
import org.fermat.FermatTextNode
import org.fermat.FermatGeneralNodeLike
import org.fermat.FermatText
import org.fermat.FermatGeneralAttribute
import org.fermat.FermatEventAttribute
import org.fermat.FermatDynamicText
import org.fermat.FermatStaticText

object Html {
  def apply(component: Component, getComponent: String => Component): HtmlComponent = {
    component.viewImpl match {
      case DefaultViewImpl(Template(nodes), optScript) => {
        val inner = HtmlInnerNodes(nodes.map { child =>
          apply(child, getComponent)
        })
        val script = optScript.map(_.value).getOrElse("")
        HtmlComponent(component, HtmlDefaultViewImpl(inner, script))
      }
      case NonLimitViewImpl(Script(script)) => {
        HtmlComponent(component, HtmlNonLimitViewImpl(script))
      }
    }
  }
  def apply(node: FermatGeneralNodeLike, getComponentByName: String => Component): Html = {
    node match {
      case node: FermatComponentNode => {
        val component = getComponentByName(node.label)
        val children = node.children.map { child =>
          toTranscludeArgNode(child, component, getComponentByName)
        }.flatten
        HtmlComponentNode(node, children, component)
      }
      case node: FermatGeneralNode => {
        //        if (node.label == "transclude") {
        //          HtmlTranscludeTargetNode(node)
        //        } else {
        val inner = node.children match {
          case Seq(FermatTextNode(text)) => {
            HtmlInnerText(text)
          }
          case children => {
            HtmlInnerNodes(children.flatMap { c =>
              c match {
                case c: FermatGeneralNodeLike => {
                  Some(apply(c, getComponentByName))
                }
                case _ => {
                  None
                }
              }
            })
          }
        }
        HtmlNode(node, inner)
      }
    }

  }

  def toTranscludeArgNode(node: FermatGeneralNodeLike, component: Component, getComponentByName: String => Component): Option[HtmlTranscludeArgNode] = {
    component.transcludeArgsAsMap.get(node.label) map {
      targ =>
        HtmlTranscludeArgNode(node, HtmlInnerNodes(node.children.flatMap { c =>
          c match {
            case c: FermatGeneralNodeLike => {
              Some(apply(c, getComponentByName))
            }
            case _ => {
              None
            }
          }
        }))
    }
  }

  def toHtmlString(htmlNode: HtmlNode): String = { //TODO
    htmlNode.node match {
      case n: FermatGeneralNodeLike => {
        val attributes = (n.attributes.flatMap { attr =>
          attr match {
            case FermatGeneralAttribute(key, value) => value match {
              case FermatStaticText(text) => Some(s"""${key}="${text}"""")
              case _ => None
            }
            case FermatEventAttribute(_) => None
          }
        }).mkString(" ")
        s"<${n.label} ${attributes}/>"
      }
      case _ => ""
    }
  }
  def toHtmlString(htmlNode: HtmlTranscludeArgNode): String = {
    htmlNode.node match {
      case n: FermatGeneralNodeLike => {
        val attributes = (n.attributes.flatMap { attr =>
          attr match {
            case FermatGeneralAttribute(key, value) => value match {
              case FermatStaticText(text) => Some(s"""${key}="${text}"""")
              case _ => None
            }
            case FermatEventAttribute(_) => None
          }
        }).mkString(" ")
        s"<${n.label} ${attributes}/>"
      }
      case _ => ""
    }
  }
  def classNameOf(component: Component): String = component.node.attribute("name").get.toString.capitalize

}
case class HtmlComponent(component: Component, viewImpl: HtmlViewImpl)

sealed abstract class HtmlViewImpl
case class HtmlDefaultViewImpl(template: HtmlInnerNodes, script: String) extends HtmlViewImpl{
//  template.value.foreach {
//    v => println(v.getClass())
//  }
}
case class HtmlNonLimitViewImpl(script: String) extends HtmlViewImpl

sealed abstract class HtmlInner
case class HtmlInnerText(text: FermatText) extends HtmlInner
case class HtmlInnerNodes(value: Seq[Html]) extends HtmlInner

sealed abstract class Html
sealed abstract class HtmlWithInner extends Html {
  def node: FermatGeneralNodeLike
  def inner: HtmlInner
}
case class HtmlNode(node: FermatGeneralNode, inner: HtmlInner) extends HtmlWithInner
case class HtmlTranscludeTargetNode(name: String) extends Html
case class HtmlTranscludeArgNode(node: FermatGeneralNodeLike, inner: HtmlInner) extends HtmlWithInner
case class HtmlComponentNode(node: FermatComponentNode, children: Seq[HtmlTranscludeArgNode], component: Component) extends Html











