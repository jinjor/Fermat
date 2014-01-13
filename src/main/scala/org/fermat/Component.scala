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
import scala.util.parsing.combinator.RegexParsers
import scala.xml.Text

object Component {

  abstract sealed class FermatException {
    def cause: Exception
  }
  case class FermatValidationException(cause: Exception) extends FermatException
  case class FermatFileNotFoundException(cause: Exception) extends FermatException

  def apply(jarPath: String, path: String): Either[FermatException, Component] = {
    val node = Dao.loadXml(path)
    Component.validate(jarPath + "/fermat.xsd", path) match {
      case Right(_) => {
        val requires = (node \ "require").map(xml => Require(xml.attribute("src").get.toString))
        val requiresMap = (requires.map { r =>
          (r.name, r)
        }).toMap

        val args = (node \ "arg")
        val transcludeArgs = (args.filter { a =>
          a.attribute("transclude") match {
            case Some(attr) if (attr.toString == "true") => true
            case None => false
          }
        }).map(xml => TranscludeArg(xml.attribute("name").get.toString))

        val templates = (node \ "template")
        val templateOpt = if (templates.isEmpty) None else Some(templates(0))
        val viewImpl = templateOpt match {
          case Some(template) => {
            val multi = template.child.length > 1
            val nodes = template.child.flatMap { c =>
              if(multi && c.isInstanceOf[Text]){
                None
              }else{
                Some(FermatNode(c, requiresMap.contains _).asInstanceOf[FermatGeneralNodeLike])
              }
            }
            val scriptNode = (node \ "script").headOption.map {
              scriptNode => Script(scriptNode.text)
            }
            DefaultViewImpl(Template(nodes), scriptNode)
          }
          case None => NonLimitViewImpl(Script((node \ "script").head.child(0).toString))
        }
        val componentName = node.attribute("name").get.toString
        Right(Component(componentName, requires, transcludeArgs, viewImpl))
      }
      case Left(ex) => {
        ex.printStackTrace()
        Left(FermatValidationException(ex))
      }
    }
  }

  def validate(xsdFile: String, xmlFile: String): Either[Exception, Unit] = {
    try {
      val schemaLang = "http://www.w3.org/2001/XMLSchema"
      val factory = SchemaFactory.newInstance(schemaLang)
      val schema = factory.newSchema(new StreamSource(xsdFile))
      val validator = schema.newValidator()
      validator.validate(new StreamSource(xmlFile)) // TODO read many times...
      Right()
    } catch {
      case ex: Exception => Left(ex)
    }
  }
}

case class Component(name: String, requires: Seq[Require], transcludeArgs: Seq[TranscludeArg],
  viewImpl: ViewImplementation) {
  lazy val transcludeArgsAsMap = transcludeArgs.map(ta => (ta.name, ta)).toMap
}
abstract sealed class ViewImplementation
case class DefaultViewImpl(template: Template, script: Option[Script]) extends ViewImplementation
case class NonLimitViewImpl(script: Script) extends ViewImplementation

case class Require(src: String) {
  lazy val name: String = RequireParser(src)
}
case class Template(nodes: Seq[FermatGeneralNodeLike])
case class Script(value: String){
  //println(value)
}
case class TranscludeArg(name: String)

object RequireParser extends RegexParsers {//TODO
  def componentName: Parser[String] = """[.*\.]*""".r ~ """[a-z]+""".r ~ ".xml".r ^^ {
    case _ ~ exp ~ _ => exp
  }
  def apply(input: String): String = parseAll(componentName, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }
}
    

