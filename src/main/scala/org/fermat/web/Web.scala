package org.fermat.web

import scala.xml.Node
import org.fermat.Component
import org.fermat.Event
import org.fermat.util.Output
import org.fermat.Dependency

object Web {

  def all(js: JavaScript, root: String, deps: List[Component]): Seq[Output] = {
    val componentMap = Map[String, Component]()
    val (_, _htmlDeps) = (deps.foldLeft((componentMap, List[HtmlComponent]())) {
      case ((componentMap, htmlDeps), component) =>
        val html = Html(component, (name: String) => componentMap.get(name).get)
        (componentMap + (component.name -> component), html :: htmlDeps)
    })
    val htmlDeps = _htmlDeps.reverse
    val htmlComponent = htmlDeps.last

    val wholeScript = js.makeHeaderScript(htmlDeps, htmlComponent)

    val html = s"""
      <html>
      	<head>
      		${js.preLoadTagsAsString}
    		$wholeScript
        </head>
      	<body>
        </body>
      </html>"""

    val subOutputs = js.makeSubOutput match {
      case Some(f) => htmlDeps.map(f)
      case None => Seq()
    }
    subOutputs :+ Output(s"${root}/out/index.html", html)
  }
}