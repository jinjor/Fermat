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

object Polymer extends JavaScript{

  lazy val preLoadTagsAsString: String = { //TODO
    """<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
        <script src="./lib/jquery-2.0.3.min.js"></script>
    	<script src="./lib/underscore-min.js"></script>
        <script src="./lib/platform.js"></script>"""
  }
  
  private def xmlPathTohtmlPath(xmlPath: String): String = {
    s"""${xmlPath.replace(".xml", ".html")}"""
  }
  private def linkTag(path: String): String = {
    s"""<link rel="import" src="${path}"><link>"""
  }
  private def reqToLinkTag(req: Require): String = {
    linkTag(xmlPathTohtmlPath(req.src))
  }
  

  def makeHeaderScript(htmlDeps: Seq[HtmlComponent], topComponent: HtmlComponent):String = {
    val classDefs = htmlDeps.map { component =>
      Backbone.clazzDef(component)
    }
    val linkTags = topComponent.component.requires.map(reqToLinkTag)
    linkTags.mkString("\n")
  }
  
  private def templateTag(component: HtmlComponent): String = {
    component.viewImpl match {
      case HtmlDefaultViewImpl(template, script) => {
        val noScript = if(script.trim.isEmpty) "noscript" else ""
        s"""<template $noScript>
        $template
        </template>"""
      }
      case HtmlNonLimitViewImpl(_) => ""
    }
  }
  
  def makeSubOutput: Option[HtmlComponent => Output] = Some { subComponent =>
    val path = s"""${subComponent.component.name}.html"""
    
    val linkTags = subComponent.component.requires.map(reqToLinkTag)
    val content = s"""
    <element name="${subComponent.component.name}">
    	${templateTag(subComponent)}
    </element>
    """
    
    Output(path, content)
  }


}