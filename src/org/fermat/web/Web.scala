package org.fermat.web

import scala.xml.Node
import org.fermat.Component
import org.fermat.Event
import org.fermat.util.Output
import org.fermat.Tsort.{ Node => TNode }

object Web {

  def all(top: Component): Output = {
    val htmlComponent = Html(top)

    val script = JavaScript.expression(htmlComponent)
    val classDefs = deps(htmlComponent).map { component =>
      JavaScript.clazzDef(component)
    }

    val wholeScript = makeWholeScript(classDefs, script)

    val html = s"""
      <html>
      	<head>
      		${JavaScript.preLoadTagsAsString}
    		$wholeScript
        </head>
      	<body>
        </body>
      </html>"""

    Output("sandbox/out.html", html)
  }

  def makeWholeScript(classDefs: Seq[String], instanceScript: String) = s"""<script>
  	$$(function(){
		${classDefs.mkString("\n")}
  	    $$('body').html(${instanceScript});
  	});
  </script>"""

  def deps(top: HtmlComponent): Seq[HtmlComponent] = {
    val getComponent: String => Component = (src: String) => {
      Component(src).right.get
    }
	val tNodeList = makeNodesForTsort(top.component, Map(), getComponent, Nil)
    List(top)
  }

  def makeNodesForTsort(top: Component, cache: Map[String, Component], getComponent: String => Component,
      tNodeList: List[TNode[Component]]): (Map[String, Component], List[TNode[Component]]) = {
    val (newCache, reqComponents) = top.requires.foldLeft((cache, List[Component]())) {
      case ((memo, list), req) =>
        val src = req.node.attribute("src").toString
        val component = memo.get(src) match {
          case Some(component) => component
          case None => getComponent(src)
        }
        (memo + (src -> component), component :: list)
    }
    val init = (newCache, TNode(top, reqComponents) :: tNodeList)
    reqComponents.foldLeft(init){ case((cache, list), reqComponent) =>
      makeNodesForTsort(reqComponent, cache, getComponent, list)
    }
  }

}