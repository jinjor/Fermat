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
    val bodyInner = js.bodyInnerHtml(htmlComponent)
    val html = s"""
      <html>
      	<head>
      		${js.preLoadTagsAsString}
    		$wholeScript
        </head>
      	<body>
    		${bodyInner}
        </body>
      </html>"""
    val outDir = s"${root}/out"
    val subOutputs = js.makeSubOutput.map { f =>
      htmlDeps.map { deps =>
        f(outDir, deps)
      }
    }.getOrElse(Seq())
    subOutputs :+ Output(s"${outDir}/index.html", html)
  }
}