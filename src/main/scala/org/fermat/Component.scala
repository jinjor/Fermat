package org.fermat

import javax.xml.transform.stream.StreamSource
import javax.xml.validation.Schema
import javax.xml.validation.SchemaFactory
import javax.xml.validation.{ Validator => JValidator }
import scala.xml.Elem
import scala.xml.Node
import scala.xml.Attribute
import scala.xml.MetaData
import org.fermat.util.Dao

object Component {

  abstract sealed class FermatException {
    def cause: Exception
  }
  case class FermatValidationException(cause: Exception) extends FermatException
  case class FermatFileNotFoundException(cause: Exception) extends FermatException

  def apply(jarPath:String, path: String): Either[FermatException, Component] = {
    val node = Dao.loadXml(path)
    Component.validate(jarPath + "/fermat.xsd", path) match {
      case Right(_) => {
        val requires = (node \ "require").map(xml => Require(xml))
        val args = (node \ "arg")
        val transcludeArgs = (args.filter { a =>
          a.attribute("transclude") match {
            case Some(attr) if(attr.toString == "true") => true
            case None => false
          }
        }).map(xml => TranscludeArg(xml))
        
        val template = (node \ "template")(0)
        val script = (node \ "script").headOption
        Right(Component(node, requires, transcludeArgs, Template(template), Script(script)))
      }
      case Left(ex) => Left(FermatValidationException(ex))
    }
  }

  def validate(xsdFile: String, xmlFile: String): Either[Exception, Unit] = {
    try {
      val schemaLang = "http://www.w3.org/2001/XMLSchema"
      val factory = SchemaFactory.newInstance(schemaLang)
      val schema = factory.newSchema(new StreamSource(xsdFile))
      val validator = schema.newValidator()
      validator.validate(new StreamSource(xmlFile))// TODO read many times...
      Right()
    } catch {
      case ex: Exception => Left(ex)
    }
  }

}
case class Component(node: Node, requires: Seq[Require], transcludeArgs:Seq[TranscludeArg],
    template: Template, script: Script){
  lazy val transcludeArgsAsMap = transcludeArgs.map(ta => (ta.node.attribute("name").get.toString -> ta)).toMap
}

abstract sealed class FermatNode
case class Require(node: Node) extends FermatNode
case class Template(node: Node) extends FermatNode
case class Script(node: Option[Node]) extends FermatNode
case class TranscludeArg(node: Node) extends FermatNode









