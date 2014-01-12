package org.fermat.web

import scala.xml.Node
import org.fermat.Component
import org.fermat.Event
import org.fermat.util.Output
import org.fermat.Dependency

object Web {

  def all(js: JavaScript, root: String, deps: List[Component]): Output = {
    val componentMap = Map[String, Component]()
    val (_, _htmlDeps) = (deps.foldLeft((componentMap, List[HtmlComponent]())) {
      case ((componentMap, htmlDeps), component) =>
        val html = Html(component, (name: String) => componentMap.get(name).get)
        (componentMap + (component.node.attribute("name").get.toString -> component), html :: htmlDeps)
    })
    val htmlDeps = _htmlDeps.reverse
    val htmlComponent = htmlDeps.last

    val wholeScript = makeHeaderScript(js, htmlDeps, htmlComponent)

    val html = s"""
      <html>
      	<head>
      		${js.preLoadTagsAsString}
    		$wholeScript
        </head>
      	<body>
        </body>
      </html>"""

    Output(s"${root}/out/index.html", html)
  }

  private def makeHeaderScript(js: JavaScript, htmlDeps: Seq[HtmlComponent], topComponent: HtmlComponent) = {
    s"""<script>
  	${js.makeHeaderScript(htmlDeps, topComponent)}
    </script>"""
  }

}