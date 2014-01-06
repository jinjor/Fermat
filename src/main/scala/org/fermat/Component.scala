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
            val node = FermatNode(template, requiresMap.contains _)
            val nodes = template.child.flatMap { c =>
              FermatNode(template, requiresMap.contains _) match {
                case node: FermatGeneralNodeLike => Some(node)
                case _ => None
              }
            }
            val scriptNode = (template \ "script").headOption.map {
              scriptNode => Script(scriptNode.child.toString)
            }
            node match {
              case node: FermatGeneralNode => DefaultViewImpl(Template(nodes), scriptNode)
              case _ => {
                throw new IllegalArgumentException()
              }
            }
          }
          case None => NonLimitViewImpl(Script((node \ "script").head.child.toString))
        }
        Right(Component(node, requires, transcludeArgs, viewImpl))
      }
      case Left(ex) => {
        ex.printStackTrace()
        Left(FermatValidationException(ex))
      }
    }
  }

  //    def apply(jarPath:String, path: String): Either[FermatException, Component] = {
  //    Component.validate(jarPath + "/fermat.xsd", path) match {
  //      case Right(_) => {
  //        val node = FermatNode(Dao.loadXml(path)).asInstanceOf[FermatGeneralNode]
  //        
  //        val requires = (node \ "require").map{
  //          xml =>
  //            val src = xml.asInstanceOf[FermatAtomNode].attribute("src").get.value.asInstanceOf[FermatStaticText].value
  //            Require(src)
  //        }
  //        val args = (node \ "arg").map(_.asInstanceOf[FermatAtomNode])
  //        val transcludeArgs = (args.filter { a =>
  //          a.attribute("transclude") match {
  //            case Some(attr) if(attr.toString == "true") => true
  //            case None => false
  //          }
  //        }).map(xml => TranscludeArg(xml.attribute("name").get.value.asInstanceOf[FermatStaticText].value))
  //        
  //        val templates = (node \ "template")
  //        val templateOpt = if(templates.isEmpty) None else Some(templates(0))
  //        val viewImpl = templateOpt match {
  //          case Some(template) => DefaultViewImpl(Template(template), (node \ "script").headOption.map { script =>
  //            Script(script.asInstanceOf[FermatTextNode].text.asInstanceOf[FermatStaticText].value)
  //          })
  //          case None => NonLimitViewImpl(Script((node \ "script").head))
  //        }
  //        Right(Component(node, requires, transcludeArgs, viewImpl))
  //      }
  //      case Left(ex) => {
  //        ex.printStackTrace()
  //        Left(FermatValidationException(ex))
  //      }
  //    }
  //  }

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

case class Component(node: Node, requires: Seq[Require], transcludeArgs: Seq[TranscludeArg],
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
case class Script(value: String)
case class TranscludeArg(name: String)

object RequireParser extends RegexParsers {
  def componentName: Parser[String] = """.*""" ~ """[a-z]""" ~ ".xml".r ^^ {
    case _ ~ exp ~ _ => exp
  }
  def apply(input: String): String = parseAll(componentName, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }
}
    

