package fermat

import scala.xml.Node

object Web {
  
  def all(top: HtmlComponent): Output = {
    val script = JavaScript.expression(top)
    val classDefs = deps(top).map { component =>
      JavaScript.clazzDef(component)
    }
    
    val wholeScript = s"""<script>
      	$$(function(){
    		${classDefs.mkString("\n")}
      	    $$('body').html(${script});
      	});
      </script>"""
    
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
  
  def deps(top: HtmlComponent): Seq[HtmlComponent] = {
    List(top)
  }

  //--------------------------
  object Html {

    def apply(component: Component/*, customs: Map[String, Component]*/): HtmlComponent = {
      val template = apply(component.template.node)
      val script = component.script.node.map(node => node.text).getOrElse("")
      HtmlComponent(component, template, script)
    }
    def apply(node: Node): HtmlNode = {
      HtmlNode(node, node.child.map { child =>
        apply(child)
      })
    }
    
  }

  abstract class Html
  case class HtmlNode(node: Node, children: Seq[Html]) extends Html {
    def toHtmlString: String = {
      if (node.isAtom) node.text.trim else {
        //val attributes = node.attributes
        
        //s"<${node.label}></${node.label}>"
        node.toString
      }
    }
    //override def toString: String = ""
  }
  case class HtmlComponent(component: Component, template: HtmlNode, script: String) extends Html {
    lazy val className = component.node.label
    //override def toString: String = ""
  }

  //-----------
  object JavaScript {
    
    lazy val preLoadTagsAsString: String = {
      """<script src="../lib/jquery-2.0.3.min.js"></script>
    	<script src="../lib/underscore-min.js"></script>
    	<script src="../lib/backbone-min.js"></script>"""
    }
    
    
    def expression(html: Html): String = {
      html match {
        case html: HtmlNode => expression(html)
        case html: HtmlComponent => expression(html)
      }
    }
    def expression(html: HtmlNode): String = {
      val s = html.toHtmlString
      if (s.isEmpty) "" else s"""$$('$s')""" + (html.children.map { child =>
        val s = JavaScript.expression(child)
        if (s.isEmpty) "" else s"""\n.append($s)"""
      }).mkString
    }
    def expression(html: HtmlComponent): String = {
      s"$$(new ${html.className}().$$el)" //TODO args
    }
    def clazz(htmlComponent: HtmlComponent): String = {
      val div = HtmlNode(<div/>, htmlComponent.template.children)
      val s = expression(div)
      
      s"""
	Backbone.View.extend({
	  initialize: function(){
	    this.$$el = $$($s);
	  }
	})"""
    }
    def clazzDef(htmlComponent: HtmlComponent): String = {
      s"var ${htmlComponent.className} = ${clazz(htmlComponent)};"
    }
  }


}