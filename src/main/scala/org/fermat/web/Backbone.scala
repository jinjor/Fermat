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

object Backbone extends JavaScript{

  private abstract sealed class ElementProfWithInnerData extends ElementProf {
    def innerData: InnerData[ElementProf]
  }
  private abstract sealed class ElementProf {
    def elementVarName: String
  }
  private case class GeneralProf(elementVarName: String, elementExpression: String,
    innerData: InnerData[ElementProf], events: Iterable[Event]) extends ElementProfWithInnerData
  private case class TranscludeArgProf(elementVarName: String, elementExpression: String,
    innerData: InnerData[ElementProf], name: String) extends ElementProfWithInnerData
  private case class TranscludeTargetProf(elementVarName: String, elementExpression: String,
    name: String) extends ElementProf

  private case class ComponentProf(componentVarName: String, elementVarName: String,
    componentExpression: String, children: Seq[TranscludeArgProf]) extends ElementProf

  lazy val preLoadTagsAsString: String = { //TODO
    """<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
        <script src="./lib/jquery-2.0.3.min.js"></script>
    	<script src="./lib/underscore-min.js"></script>
    	<script src="./lib/backbone-min.js"></script>
	  <script src="./lib/Bacon.min.js"></script>"""
  }
  
  private lazy val eventAttrToString = (e: Event) => ""

  private def toInnerData(html: HtmlWithInner): InnerData[ElementProf] = html.inner match {
    case HtmlInnerText(text) => text match {
      case FermatStaticText(text) => InnerStaticText(text)
      case FermatDynamicText(_, modelNames) => InnerDynamicText(modelNames)
    }
    case HtmlInnerNodes(children) => { //TODO
      if (children.isEmpty) {
        html.node.attribute("data") match {
          case Some(FermatGeneralAttribute(key, value)) => if (kindOfInput(html)) {
            println(html.node.label);
            InnerInputText(value.asInstanceOf[FermatDynamicText].modelNames)
          } else {
            value match {
              case FermatStaticText(text) => InnerStaticText(text)
              case FermatDynamicText(_, modelNames) => InnerDynamicText(modelNames)
            }
          }
          case None => {
            println(html)
            InnerStaticText("")
          }
        }
      } else {
        InnerElements(children.flatMap { child =>
          elementProf(child)
        })
      }
    }
  }

  private def toTranscludeArgProf(html: HtmlTranscludeArgNode): TranscludeArgProf = {
    val s = Html.toHtmlString(html, None, eventAttrToString)
    val elementExpression = s"""$$('$s')"""

    val innerData = toInnerData(html)
    val elementVarName = crateNewVarName()
    TranscludeArgProf(elementVarName, elementExpression, innerData, html.node.label)
  }

  private def elementProf(html: Html): Option[ElementProf] = html match {
    case html: HtmlNode => {
      val s = Html.toHtmlString(html, None, eventAttrToString)
      if (s.isEmpty) {
        None
      } else {
        val elementExpression = s"""$$('$s')"""
        val innerData = toInnerData(html)
        val events = html.node.events.map(_.event)
        val elementVarName = crateNewVarName()
        Some(GeneralProf(elementVarName, elementExpression, innerData, events))
      }
    }
    case html: HtmlTranscludeArgNode => { //
      Some(toTranscludeArgProf(html))
    }
    case html: HtmlTranscludeTargetNode => { //TODO refactor
      val elementVarName = crateNewVarName()
      val elementExpression = s"""$$('<div/>')"""
      Some(TranscludeTargetProf(elementVarName, elementExpression, html.name))
    }
    case html: HtmlComponentNode => { //
      val childrenProf = html.children.map(toTranscludeArgProf)

      val keyValues = for {
        child <- childrenProf
        expr = expression(child)
        (name, declarations) = renderScriptsForTranscludeArg(child)
      } yield s"""'${name}': function(scope){//child scope including parents'
    	    ${declarations.mkString("\n")}
    	    var a = ${expr};
    	    //console.log(scope);
    	    //console.log(a.html());
    	    return a.children();
          }"""

      val args = (html.node.attributes.flatMap { attr =>
        attr match {
          case FermatGeneralAttribute(key, value) => value match {
            case FermatDynamicText(_, modelNames) =>
              Some(s"""get ${key}(){return scope.${modelNames.head}; },
    	          set ${key}(v){ scope.${modelNames.head} = v; }""")//TODO template
            case FermatStaticText(text) => Some(s"""${key}: '${text}'""")
          }
          case _ => None
        }
      })
      val arg = s"""{
      	${(args ++ keyValues).mkString(",\n")}
      }""" //TODO name
      
      println(args)
      val componentExpression = s"""new ${Html.classNameOf(html.component)}(${arg})"""
      val innerData: InnerData[ElementProf] = InnerDataCapsuled
      val componentVarName = crateNewVarName()
      val elementVarName = crateNewVarName()
      Some(ComponentProf(componentVarName, elementVarName, componentExpression, childrenProf))
    }
  }

  private def innerDataAppend(prof: ElementProfWithInnerData): String = {
    (prof.innerData match {
      case InnerElements(children) => (children.map { child =>
        val s = expression(child)
        if (s.isEmpty) "" else s"""\n.append($s)"""
      }).mkString
      case InnerStaticText(text) => s"""\n.text("$text")"""
      case InnerDynamicText(modelNames) => s"""\n.text(scope.${modelNames.head})"""//FIXME template
      case _ => ""
    })
  }

  private def expression(prof: ElementProf): String = {
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

  def makeHeaderScript(htmlDeps: Seq[HtmlComponent], topComponent: HtmlComponent) = {
    val classDefs = htmlDeps.map { component =>
      Backbone.clazzDef(component)
    }
    s"""
    <script>
  	$$(function(){
		${classDefs.mkString("\n")}
  		var top = new ${Html.classNameOf(topComponent.component)}({});
  		top.on("update", function(){
  			top.render();
  		});
  	    $$('body').html(top.$$el.children());
  	});
  	</script>"""
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

  private def renderScriptsForInner(elementVarName: String, innerData: InnerData[ElementProf]): Seq[String] = {
    innerData match {
      case InnerDynamicText(modelVarName) =>
        List(s"${elementVarName}.text(scope.${modelVarName})")
      case InnerInputText(modelVarName) => {
        List(s"""!${elementVarName}.is(":focus") && ${elementVarName}.val(scope.${modelVarName})""")
      }
      case InnerElements(children) =>
        children.flatMap(renderScripts)
      case _ => Nil
    }
  }

  private def renderScripts(ep: ElementProf): Seq[String] = {
    ep match {
      case prof: GeneralProf => renderScriptsForInner(prof.elementVarName, prof.innerData)
      case prof: TranscludeArgProf => renderScriptsForInner(prof.elementVarName, prof.innerData)
      case prof: TranscludeTargetProf => {
        List(s"${prof.elementVarName}.html(scope.${prof.name}(scope))") //TODO replace?
      }
      case prof: ComponentProf => List(s"${prof.componentVarName}.render()")
    }
  }
  private def renderScriptsForTranscludeArg(transcludeArg: TranscludeArgProf): (String, Seq[String]) = {
    (transcludeArg.name -> (elementDeclarations(transcludeArg) ++ renderScripts(transcludeArg)))
  }

  private def clazz(htmlComponent: HtmlComponent): String = {

    val userScript = htmlComponent.viewImpl match {
      case HtmlDefaultViewImpl(template, script) => {
        val div = HtmlNode(FermatGeneralNode("div", Iterable(), Seq()), template)
        val prof = elementProf(div).get
        val expr = expression(prof)
        //println(expr)
        val elementDeclaration = elementDeclarations(prof).mkString("\n")
        val renderScriptStr = renderScripts(prof).mkString("\n")
        s"""
        ${script}
    	${elementDeclaration}
    	this.$$el.html($$($expr));
    	self.render = function(){
    	    ${renderScriptStr}
    	};
        """
      }
      case HtmlNonLimitViewImpl(script) => script
    }

    s"""
	Backbone.View.extend(_.extend({
	  initialize: function(scope){
    	var self = this;
        ${userScript}
	    self.render();
	  }
	}, Backbone.Events))"""
  }
  def clazzDef(htmlComponent: HtmlComponent): String = {
    s"var ${Html.classNameOf(htmlComponent.component)} = ${clazz(htmlComponent)};"
  }
  
  def makeSubOutput = None
}