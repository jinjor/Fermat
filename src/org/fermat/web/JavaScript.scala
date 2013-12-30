package org.fermat.web

import org.fermat.Event

object JavaScript {

  sealed abstract class InnerData
  case class InnerElements(children: Seq[ElementProf]) extends InnerData
  case class InnerStaticText(text: String) extends InnerData
  case class InnerDynamicText(modelName: String) extends InnerData
  case object InnerDataCapsuled extends InnerData

  abstract class ElementProf {
    def elementVarName: String
    def  events: Iterable[Event]
  }
  case class GeneralProf(elementVarName: String, elementExpression: String,
    innerData: InnerData, events: Iterable[Event]) extends ElementProf
  case class ComponentProf(componentVarName: String, elementVarName: String,
      componentExpression: String) extends ElementProf {
    val events = Nil
  }

  lazy val preLoadTagsAsString: String = {
    """<script src="../lib/jquery-2.0.3.min.js"></script>
    	<script src="../lib/underscore-min.js"></script>
    	<script src="../lib/backbone-min.js"></script>
	  <script src="../lib/Bacon.min.js"></script>"""
  }
  /*
  def expression(prof: ElementProf): String = {
    html match {
      case html: HtmlNode => expression(html)
      case html: HtmlComponent => expression(html)
    }
  }
  */
  var id = 0
  def crateNewVarName(): String = {
    id = id + 1
    "var" + id;
  }
  
  

  def elementProf(html: Html): Option[ElementProf] = html match {
    case html: HtmlNode => {
      val s = Html.toHtmlString(html)
      if (s.isEmpty) {
        None
      }else{
        val elementExpression = s"""$$('$s')"""
        val innerData: InnerData =
          if (html.children.isEmpty) {
            html.node.attribute("data") match {
              case Some(attr) => InnerDynamicText(attr.toString)
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
    case html: HtmlComponentNode => {//
    	val args = (html.node.attributes.map { attr =>
    	  s"get ${attr.key}(){ return scope.${attr.value} }"
    	}).mkString(",\n")
    	val arg = s"{\n${args}}"
        val componentExpression = s"""new ${Html.classNameOf(html.component)}(${arg})"""
        val innerData: InnerData = InnerDataCapsuled
        val events = Event.eventsOf(html.node)
        val componentVarName = crateNewVarName()
        val elementVarName = crateNewVarName()
        Some(ComponentProf(componentVarName, elementVarName, componentExpression))
    }
  }
  
  def innerDataAppend(prof: ElementProf): String = {
    prof match {
      case prof: GeneralProf => {
	      (prof.innerData match {
	      case InnerElements(children) => (children.map { child =>
	        val s = JavaScript.expression(child)
	        if (s.isEmpty) "" else s"""\n.append($s)"""
	      }).mkString
	      case _ => ""
	    })
      }
      case prof: ComponentProf => ""
    }
  }

  def expression(prof: ElementProf): String = {
    val s2 = s"""$$(${prof.elementVarName})${innerDataAppend(prof)}"""
    val events = prof.events
    events.foldLeft(s2) { (memo, event) =>
      s"""${memo}.on('${event.name}', function(){
     ${event.methodName}(this);
     self.render();
    })"""
    }
  }

  def expression(html: HtmlComponent): String = {
    s"new ${Html.classNameOf(html.component)}({}).$$el" //TODO args
  }
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
      case prof: ComponentProf => {
        val ownDeclaration = s"""var ${prof.componentVarName} = ${prof.componentExpression};"""
        val ownElementDeclaration = s"""var ${prof.elementVarName} = ${prof.componentVarName}.$$el;"""
        List(ownDeclaration, ownElementDeclaration)
      }
    }
  }

  def renderScripts(ep: ElementProf): Seq[String] = {
    ep match {
      case prof: GeneralProf => (prof.innerData match {
	      case InnerDynamicText(modelVarName) =>
	        List(s"${prof.elementVarName}.text(scope.${modelVarName})")
	      case InnerElements(children) =>
	        children.flatMap(renderScripts)
	      case _ => Nil
	    })
      case prof: ComponentProf => List(s"${prof.componentVarName}.render()")
    }
  }

  def clazz(htmlComponent: HtmlComponent): String = {
    val div = HtmlNode(<div/>, htmlComponent.template.children)
    val prof = elementProf(div).get
    val expr = expression(prof)
    val elementDeclaration = elementDeclarations(prof).mkString("\n")
    val renderScriptStr = renderScripts(prof).mkString("\n")

    //val events = allEvents(htmlComponent.template)
    /*
    val eventFunctions = events.map { e =>
      s"""${e.methodName}: function(){
      alert("${e.methodName}")
    }"""
    }.mkString(",")
*/
    val userScript = htmlComponent.script
    s"""
	Backbone.View.extend(_.extend({
	  initialize: function(scope){
    	var self = this;
        ${userScript}
    	${elementDeclaration}
    	this.$$el.html($$($expr));
    	self.render = function(){
	    	if(scope.model){//TODO
    			self.trigger('update');
			}else{
			    ${renderScriptStr}
			}
    	};
	    self.render();
	  }
	}, Backbone.Events))"""
  }
  def clazzDef(htmlComponent: HtmlComponent): String = {
    s"var ${Html.classNameOf(htmlComponent.component)} = ${clazz(htmlComponent)};"
  }
  /*
  def allEvents(htmlNodes: Seq[HtmlNode]): Seq[Event] = {
    htmlNodes.flatMap(allEvents)
  }
  def allEvents(html: HtmlNode): Iterable[Event] = {
    for {
      event <- (Event.eventsOf(html.node) ++ allEvents(html.children)).seq
    } yield event
  }
  * 
  */

}