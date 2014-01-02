package org.fermat.web

import org.fermat.Event

object JavaScript {

  sealed abstract class InnerData
  case class InnerElements(children: Seq[ElementProf]) extends InnerData
  case class InnerStaticText(text: String) extends InnerData
  case class InnerDynamicText(modelName: String) extends InnerData
  case class InnerInputText(modelName: String) extends InnerData
  case object InnerDataCapsuled extends InnerData

  abstract sealed class ElementProf {
    def elementVarName: String
  }
  case class GeneralProf(elementVarName: String, elementExpression: String,
    innerData: InnerData, events: Iterable[Event]) extends ElementProf
   case class TranscludeArgProf(elementVarName: String, elementExpression: String,
    innerData: InnerData, events: Iterable[Event]) extends ElementProf
  case class TranscludeTargetProf(elementVarName: String, elementExpression: String,
    name: String) extends ElementProf

  case class ComponentProf(componentVarName: String, elementVarName: String,
    componentExpression: String) extends ElementProf

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

  def kindOfInput(html: HtmlNode): Boolean = {
    (html.node.label == "input" && html.node.attribute("type").get.toString.trim == "text") ||
      (html.node.label == "textarea")
  }
  def kindOfInput(html: HtmlTranscludeArgNode): Boolean = {
    (html.node.label == "input" && html.node.attribute("type").get.toString.trim == "text") ||
      (html.node.label == "textarea")
  }

  def elementProf(html: Html): Option[ElementProf] = html match {
    case html: HtmlNode => {
      val s = Html.toHtmlString(html)
      if (s.isEmpty) {
        None
      } else {
        val elementExpression = s"""$$('$s')"""
        val innerData: InnerData =
          if (html.children.isEmpty) {
            html.node.attribute("data") match {
              case Some(attr) => if (kindOfInput(html)) {
                InnerInputText(attr.toString)
              } else {
                InnerDynamicText(attr.toString)
              }
              case None => InnerStaticText(html.node.text) //TODO 
            }
          } else InnerElements(html.children.flatMap { child =>
            JavaScript.elementProf(child)
          })
        val events = Event.eventsOf(html.node)
        val elementVarName = crateNewVarName()
        Some(GeneralProf(elementVarName, elementExpression, innerData, events))
      }
    }
    case html: HtmlTranscludeArgNode => { //
      val s = Html.toHtmlString(html)
      if (s.isEmpty) {
        None
      } else {
        val elementExpression = s"""$$('$s')"""
        val innerData: InnerData =
          if (html.children.isEmpty) {
            html.node.attribute("data") match {
              case Some(attr) => if (kindOfInput(html)) {
                InnerInputText(attr.toString)
              } else {
                InnerDynamicText(attr.toString)
              }
              case None => InnerStaticText(html.node.text) //TODO 
            }
          } else InnerElements(html.children.flatMap { child =>
            JavaScript.elementProf(child)
          })
        val events = Event.eventsOf(html.node)
        val elementVarName = crateNewVarName()
        Some(TranscludeArgProf(elementVarName, elementExpression, innerData, events))
      }
    }
    case html: HtmlTranscludeTargetNode => {//TODO refactor
       val elementVarName = crateNewVarName()
      val s = Html.toHtmlString(html)
      val elementExpression = s"""$$('$s')"""
      val name = html.node.label
      Some(TranscludeTargetProf(elementVarName, elementExpression, name))
    }
    case html: HtmlComponentNode => { //
      val renderFunctions = for {
        (name, el) <- for {
          child <- html.children
          el <- JavaScript.elementProf(child)
        } yield (child.node.label, el)
        renderScripts = JavaScript.renderScripts(el)
        renderFunction = s"""function(scope){//child scope including parents'
    	  ${renderScripts.mkString("\n")}
        }"""
      } yield (name, renderFunction)
      val keyValues = renderFunctions.map {
        case (name, f) =>
          s"""'${name}': ${f}"""
      }
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
      Some(ComponentProf(componentVarName, elementVarName, componentExpression))
    }
  }

  def innerDataAppend(prof: GeneralProf): String = {
    (prof.innerData match {
      case InnerElements(children) => (children.map { child =>
        val s = JavaScript.expression(child)
        if (s.isEmpty) "" else s"""\n.append($s)"""
      }).mkString
      case _ => ""
    })
  }

  def expression(prof: ElementProf): String = {
    prof match {
      case prof: GeneralProf => {
        val s = s"""$$(${prof.elementVarName})${innerDataAppend(prof)}"""
        val events = prof.events
        events.foldLeft(s) { (memo, event) =>
          s"""${memo}.on('${event.name}', function(){
	       ${event.methodName}(this);
	       self.trigger('update')
	    })"""
        }
      }
      case prof: TranscludeArgProf => {
        val s = s"""${prof.elementVarName}"""
        s
      }
      case prof: TranscludeTargetProf => {
        val s = s"""${prof.elementVarName}"""
        s
      }
      case prof: ComponentProf => {
        val s = s"""${prof.elementVarName}"""
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
        List(s"""var ${prof.elementVarName} = ${prof.elementExpression};""")
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

  def renderScripts(ep: ElementProf): Seq[String] = {
    ep match {
      case prof: GeneralProf => (prof.innerData match {
        case InnerDynamicText(modelVarName) =>
          List(s"${prof.elementVarName}.text(scope.${modelVarName})")
        case InnerInputText(modelVarName) =>
          List(s"""!${prof.elementVarName}.is(":focus") && ${prof.elementVarName}.val(scope.${modelVarName})""")
        case InnerElements(children) =>
          children.flatMap(renderScripts)
        case _ => Nil
      })
      case prof: TranscludeArgProf =>(prof.innerData match {//TODO refactor
        case InnerDynamicText(modelVarName) =>
          List(s"${prof.elementVarName}.text(scope.${modelVarName})")
        case InnerInputText(modelVarName) =>
          List(s"""!${prof.elementVarName}.is(":focus") && ${prof.elementVarName}.val(scope.${modelVarName})""")
        case InnerElements(children) =>
          children.flatMap(renderScripts)
        case _ => Nil
      })
      case prof: TranscludeTargetProf => {
        List(s"${prof.elementVarName}.html(scope.${prof.name}(scope))")//TODO replace?
      }
      case prof: ComponentProf => List(s"${prof.componentVarName}.render()")
    }
  }

  def clazz(htmlComponent: HtmlComponent): String = {
    val div = HtmlNode(<div/>, htmlComponent.template.children)
    val prof = elementProf(div).get
    val expr = expression(prof)
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