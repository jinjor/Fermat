package org.fermat.web

import org.fermat.Event

object JavaScript {

  sealed abstract class InnerData
  case class InnerElements(children: Seq[ElementProf]) extends InnerData
  case class InnerStaticText(text: String) extends InnerData
  case class InnerDynamicText(modelName: String) extends InnerData
  case class InnerInputText(modelName: String) extends InnerData
  case object InnerDataCapsuled extends InnerData

  abstract sealed class ElementProfWithInnerData extends ElementProf {
    def innerData: InnerData
  }
  abstract sealed class ElementProf {
    def elementVarName: String
  }
  case class GeneralProf(elementVarName: String, elementExpression: String,
    innerData: InnerData, events: Iterable[Event]) extends ElementProfWithInnerData
  case class TranscludeArgProf(elementVarName: String, elementExpression: String,
    innerData: InnerData, name: String) extends ElementProfWithInnerData
  case class TranscludeTargetProf(elementVarName: String, elementExpression: String,
    name: String) extends ElementProf

  case class ComponentProf(componentVarName: String, elementVarName: String,
    componentExpression: String, children: Seq[TranscludeArgProf]) extends ElementProf

  lazy val preLoadTagsAsString: String = { //TODO
    """<script src="./lib/jquery-2.0.3.min.js"></script>
    	<script src="./lib/underscore-min.js"></script>
    	<script src="./lib/backbone-min.js"></script>
	  <script src="./lib/Bacon.min.js"></script>"""
  }

  var id = 0
  def crateNewVarName(): String = {
    id = id + 1
    "var" + id;
  }

  def kindOfInput(html: HtmlWithInner): Boolean = {
    (html.node.label == "input" && html.node.attribute("type").get.toString.trim == "text") ||
      (html.node.label == "textarea")
  }

  def toInnerData(html: HtmlWithInner): InnerData = html.inner match {
    case HtmlInnerText(text) => InnerStaticText(html.node.text)
    case HtmlInnerNodes(children) => {
      if (children.isEmpty) {
        html.node.attribute("data") match {
          case Some(attr) => if (kindOfInput(html)) {
            InnerInputText(attr.toString)
          } else {
            InnerDynamicText(attr.toString)
          }
          case None => InnerStaticText(html.node.text)
        }
      } else {
        InnerElements(children.flatMap { child =>
          JavaScript.elementProf(child)
        })
      }
    }
  }

  def toTranscludeArgProf(html: HtmlTranscludeArgNode): TranscludeArgProf = {
//    println("aaa:" + html)
    val s = Html.toHtmlString(html)
    val elementExpression = s"""$$('$s')"""
    val innerData = toInnerData(html)
    val elementVarName = crateNewVarName()
    TranscludeArgProf(elementVarName, elementExpression, innerData, html.node.label)
  }

  def elementProf(html: Html): Option[ElementProf] = html match {
    case html: HtmlNode => {
      val s = Html.toHtmlString(html)
      if (s.isEmpty) {
        None
      } else {
        val elementExpression = s"""$$('$s')"""
        val innerData = toInnerData(html)
        val events = Event.eventsOf(html.node)
        val elementVarName = crateNewVarName()
        Some(GeneralProf(elementVarName, elementExpression, innerData, events))
      }
    }
    case html: HtmlTranscludeArgNode => { //
      Some(toTranscludeArgProf(html))
    }
    case html: HtmlTranscludeTargetNode => { //TODO refactor
      val elementVarName = crateNewVarName()
      val s = Html.toHtmlString(html)
      val elementExpression = s"""$$('$s')"""
      val name = html.node.attribute("name").get.toString
      Some(TranscludeTargetProf(elementVarName, elementExpression, name))
    }
    case html: HtmlComponentNode => { //
      val childrenProf = html.children.map(toTranscludeArgProf)
      
      val keyValues = for {
        child <- childrenProf
        expr = expression(child)
        (name, declarations) = renderScriptsForTranscludeArg(child)
      } yield s"""'${name}': function(scope){//child scope including parents'
    	    ${declarations.mkString("\n")}
    	    return $expr;
          }"""

      val args = (html.node.attributes.map { attr =>
        s"""get ${attr.key}(){return scope.${attr.value}; },//TODO
    	  set ${attr.key}(v){ scope.${attr.value} = v; }"""
      })
      val arg = s"""{
      	${(args ++ keyValues).mkString(",")}
      }""" //TODO name
      val componentExpression = s"""new ${Html.classNameOf(html.component)}(${arg})"""
      val innerData: InnerData = InnerDataCapsuled
      val componentVarName = crateNewVarName()
      val elementVarName = crateNewVarName()
      Some(ComponentProf(componentVarName, elementVarName, componentExpression, childrenProf))
    }
  }

  def innerDataAppend(prof: ElementProfWithInnerData): String = {
    (prof.innerData match {
      case InnerElements(children) => (children.map { child =>
        val s = JavaScript.expression(child)
        if (s.isEmpty) "" else s"""\n.append($s)"""
      }).mkString
      case InnerStaticText(text) => s"""\n.text("$text")"""
      case InnerDynamicText(modelName) => s"""\n.text(scope.$modelName)"""
      case _ => ""
    })
  }

  def expression(prof: ElementProf): String = {
    prof match {
      case prof: GeneralProf => {
        val s = s"""$$(${prof.elementVarName})${innerDataAppend(prof)}"""
        //println("0:" + s)
        //        println("0::" + prof.innerData)
        val events = prof.events
        events.foldLeft(s) { (memo, event) =>
          s"""${memo}.on('${event.name}', function(){
	       ${event.methodName}(this);
	       self.trigger('update')
	    })"""
        }
      }
      case prof: TranscludeArgProf => {
        val s = s"""${prof.elementVarName}${innerDataAppend(prof)}"""
//        println("1:" + s)
        s
      }
      case prof: TranscludeTargetProf => {
        val s = s"""${prof.elementVarName}"""
        //println("2:" + s)
        s
      }
      case prof: ComponentProf => {
        val s = s"""${prof.elementVarName}"""
        //println("3:" + s)
        s
      }
    }

  }

  def makeWholeScript(classDefs: Seq[String], topComponent: HtmlComponent) = s"""
  	$$(function(){
		${classDefs.mkString("\n")}
  		var top = new ${Html.classNameOf(topComponent.component)}({});
  		top.on("update", function(){
  			top.render();
  		});
  	    $$('body').html(top.$$el);
  	});"""

  def elementDeclarations(prof: ElementProf): List[String] = {
    prof match {
      case prof: GeneralProf => {
        val ownDeclaration = s"""var ${prof.elementVarName} = ${prof.elementExpression};"""
        ownDeclaration :: (prof.innerData match {
          case InnerElements(children) =>
            children.toList.flatMap(elementDeclarations)
          case _ => Nil
        })
      }
      case prof: TranscludeArgProf => {
        //List(s"""var ${prof.elementVarName} = ${prof.elementExpression};""")
        val ownDeclaration = s"""var ${prof.elementVarName} = ${prof.elementExpression};"""
        ownDeclaration :: (prof.innerData match {
          case InnerElements(children) =>
            children.toList.flatMap(elementDeclarations)
          case _ => Nil
        })
      }
      case prof: TranscludeTargetProf => {
        List(s"""var ${prof.elementVarName} = ${prof.elementExpression};""")
      }
      case prof: ComponentProf => {
        val ownDeclaration = s"""var ${prof.componentVarName} = ${prof.componentExpression};"""
        val eventAttaching = s"""${prof.componentVarName}.on('update', function(){ self.trigger('update') });"""
        val ownElementDeclaration = s"""var ${prof.elementVarName} = ${prof.componentVarName}.$$el;"""
        List(ownDeclaration, eventAttaching, ownElementDeclaration)
      }
    }
  }

  def renderScriptsForInner(elementVarName: String, innerData: InnerData): Seq[String] = {
    innerData match {
      case InnerDynamicText(modelVarName) =>
        List(s"${elementVarName}.text(scope.${modelVarName})")
      case InnerInputText(modelVarName) =>
        List(s"""!${elementVarName}.is(":focus") && ${elementVarName}.val(scope.${modelVarName})""")
      case InnerElements(children) =>
        children.flatMap(renderScripts)
      case _ => Nil
    }
  }

  def renderScripts(ep: ElementProf): Seq[String] = {
    ep match {
      case prof: GeneralProf => renderScriptsForInner(prof.elementVarName, prof.innerData)
      case prof: TranscludeArgProf => renderScriptsForInner(prof.elementVarName, prof.innerData)
      case prof: TranscludeTargetProf => {
        List(s"${prof.elementVarName}.html(scope.${prof.name}(scope))") //TODO replace?
      }
      case prof: ComponentProf => List(s"${prof.componentVarName}.render()")
    }
  }
  def renderScriptsForTranscludeArg(transcludeArg: TranscludeArgProf): (String, Seq[String]) = {
    (transcludeArg.name -> (elementDeclarations(transcludeArg) ++ renderScripts(transcludeArg)))
  }

  def clazz(htmlComponent: HtmlComponent): String = {
    val div = HtmlNode(<div/>, htmlComponent.template.inner)
    val prof = elementProf(div).get
    val expr = expression(prof)
    //println(expr)
    val elementDeclaration = elementDeclarations(prof).mkString("\n")
    val renderScriptStr = renderScripts(prof).mkString("\n")
    val userScript = htmlComponent.script
    s"""
	Backbone.View.extend(_.extend({
	  initialize: function(scope){
    	var self = this;
        ${userScript}
    	${elementDeclaration}
    	this.$$el.html($$($expr));
    	self.render = function(){
    	    ${renderScriptStr}
    	};
	    self.render();
	  }
	}, Backbone.Events))"""
  }
  def clazzDef(htmlComponent: HtmlComponent): String = {
    s"var ${Html.classNameOf(htmlComponent.component)} = ${clazz(htmlComponent)};"
  }

}