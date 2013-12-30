package org.fermat.web

import scala.xml.Node
import org.fermat.Component
import org.fermat.Event
import org.fermat.util.Output
import org.fermat.Dependency

object Web {

  def all(deps: List[Component]): Output = {
    val componentMap = Map[String, Component]()
    val (_, _htmlDeps) = (deps.foldLeft((componentMap, List[HtmlComponent]())){
      case ((componentMap, htmlDeps), component) =>
        val html = Html(component, componentMap.get _)
        (componentMap + (component.node.attribute("name").get.toString -> component), html :: htmlDeps)
    })
    val htmlDeps = _htmlDeps.reverse
    val htmlComponent = htmlDeps.last

    val classDefs = htmlDeps.map { component =>
      //println(component.className)
      JavaScript.clazzDef(component)
    }
    val wholeScript = makeWholeScript(classDefs, htmlComponent)

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

  def makeWholeScript(classDefs: Seq[String], topComponent: HtmlComponent) = s"""<script>
  	${JavaScript.makeWholeScript(classDefs, topComponent)}
  </script>"""


}