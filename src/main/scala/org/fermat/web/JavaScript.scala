package org.fermat.web

import org.fermat.Event
import org.fermat.DefaultViewImpl
import org.fermat.FermatGeneralAttribute
import org.fermat.FermatDynamicText
import org.fermat.FermatStaticText
import org.fermat.FermatStaticText
import org.fermat.FermatGeneralAttribute
import org.fermat.FermatDynamicText
import org.fermat.FermatGeneralNode
import org.fermat.FermatStaticText
import org.fermat.FermatGeneralAttribute

trait JavaScript {

  sealed abstract class InnerData[+T]
  protected case class InnerElements[T](children: Seq[T]) extends InnerData[T]
  protected case class InnerStaticText(text: String) extends InnerData
  protected case class InnerDynamicText(modelName: String) extends InnerData
  protected case class InnerInputText(modelName: String) extends InnerData
  protected case object InnerDataCapsuled extends InnerData

  def preLoadTagsAsString: String

  private var id = 0
  protected def crateNewVarName(): String = {
    id = id + 1
    "var" + id;
  }

  protected def kindOfInput(html: HtmlWithInner): Boolean = {
    println(html.node.attribute("data"))
    (html.node.label == "input" && (html.node.attribute("type") match {
      case Some(FermatGeneralAttribute(_, FermatStaticText("text"))) => true
      case _ => false
    })) ||
      (html.node.label == "textarea")
  }

  def makeHeaderScript(htmlDeps: Seq[HtmlComponent], topComponent: HtmlComponent)


}