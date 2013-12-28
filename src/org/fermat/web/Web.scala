package org.fermat.web

import scala.xml.Node
import org.fermat.Component
import org.fermat.Event
import org.fermat.Output

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
    List(top)
  }


}