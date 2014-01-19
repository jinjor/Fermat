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
import org.fermat.util.Output
import org.fermat.Require
import org.fermat.FermatDynamicText
import org.fermat.FermatStaticText

object Polymer extends JavaScript{
  
  private lazy val fixLabel = (s: String) => s"fr-${s}"//TODO

  lazy val preLoadTagsAsString: String = { //TODO
    """<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
        <!-- <script src="./lib/jquery-2.0.3.min.js"></script>-->
    	<script src="./lib/underscore-min.js"></script>
        <script src="./bower_components/platform/platform.js"></script>"""
  }
  
  private def xmlPathTohtmlPath(xmlPath: String): String = {
    s"""${xmlPath.replace(".xml", ".html")}"""
  }
  private def linkTag(path: String): String = {
    s"""<link rel="import" href="${path}"></link>"""
  }
  private def reqToLinkTag(req: Require): String = {
    linkTag(xmlPathTohtmlPath(req.src))
  }
  
  def makeHeaderScript(htmlDeps: Seq[HtmlComponent], topComponent: HtmlComponent):String = {
    val classDefs = htmlDeps.map { component =>
      Backbone.clazzDef(component)
    }
    val linkTags = topComponent.component.requires.map(reqToLinkTag)
    val path = topComponent.component.name + ".html"
    (linkTags :+ linkTag(path)).mkString("\n")
  }
  
  override def bodyInnerHtml(topComponent: HtmlComponent):String = {
    val label = fixLabel(topComponent.component.name)
    s"<${label}></${label}>"
  }
  
  private def htmlAsString(htmlInner: HtmlInner): String = {
    htmlInner match {
      case HtmlInnerText(text) => text match {
        case FermatDynamicText(s) => s"""{{$s}}"""
        case FermatStaticText(s) => s
      }
      case HtmlInnerNodes(htmls) => htmls.map(htmlAsString).mkString("\n")
    }
    
  }
  private def htmlAsString(html: Html): String = {
    
    html match {
      case hwi: HtmlWithInner => {
        Html.toHtmlString(hwi.node, Some(htmlAsString(hwi.inner)), None)
      }
      case HtmlTranscludeTargetNode(name) => ""//TODO ?
      case HtmlComponentNode(node, children, component) => {
        Html.toHtmlString(node, Some(children.map(htmlAsString).mkString("\n")), Some(fixLabel))
      }
    }
  }
  private def templateTag(component: HtmlComponent): String = {
    component.viewImpl match {
      case HtmlDefaultViewImpl(template, script) => {
        val innerTags: Seq[Html] = template.value
        val htmlString = innerTags.map(htmlAsString).mkString("\n")
        
        val noScript = if(script.trim.isEmpty) "noscript" else ""
        s"""<template $noScript>
        ${htmlString}
        </template>"""
      }
      case HtmlNonLimitViewImpl(_) => ""
    }
  }
  private def scriptTag(component: HtmlComponent): String = {//FIXME
    val script = component.viewImpl match {
      case HtmlDefaultViewImpl(_, script) => {
        if(script.trim.isEmpty) None else Some(script)
      }
      case HtmlNonLimitViewImpl(s) => Some(s)
    }
    val label = fixLabel(component.component.name)
    script.map(s => s"""<script>
        var scope = {};
        $s
        Polymer('${label}', scope);
    </script>""").getOrElse("")
  }
  
  def makeSubOutput: Option[(String, HtmlComponent) => Output] = Some { (root, subComponent) =>
    val path = s"""${root}/${subComponent.component.name}.html"""
    val linkTags = subComponent.component.requires.map(reqToLinkTag).mkString("\n")
    val content = s"""
    <link rel="import" href="bower_components/polymer/polymer.html">
    <link rel="import" href="bower_components/polymer-ajax/polymer-ajax.html">
    ${linkTags}
    <polymer-element name="${fixLabel(subComponent.component.name)}">
    	${templateTag(subComponent)}
    	${scriptTag(subComponent)}
    </polymer-element>
    """
    Output(path, content)
  }


}