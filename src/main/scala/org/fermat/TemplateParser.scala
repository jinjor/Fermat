package org.fermat

import scala.collection.immutable.HashSet
import scala.util.parsing.combinator.RegexParsers

object TemplateParser extends RegexParsers {
  private def chars = """[a-zA-Z0-9:*/+\- ]*""".r
  private def intLiteral : Parser[String] =  """[1-9][0-9]*|0""".r
  private def stringLiteral : Parser[String] = "\"" ~> chars <~ "\""
  private def identifier : Parser[String] =  """[a-z]+[A-Z0-9]*""".r
  private def expr : Parser[String] = intLiteral | stringLiteral | identifier
  private def variable: Parser[String] = "{{" ~> expr <~ "}}"
  private def all: Parser[Set[String]] = opt(whiteSpace) ~> rep(variable <~ opt(chars)) <~ opt(whiteSpace) ^^ {
    variables =>
    val set: Set[String] = HashSet()
    variables.foldLeft(set){
      (set, v) => set + v
    }
  }

  def extractParamsFromString(s: String): Set[String] = {
    this.parse(all, s) match {
      case Success(result, _) => result
      case _ => {
        throw new IllegalArgumentException("cannot parse:" + s)
      }
    }
  }
}