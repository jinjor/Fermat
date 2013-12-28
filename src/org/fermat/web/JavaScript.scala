package org.fermat.web

import org.fermat.Event

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
    val s2 = if (s.isEmpty) "" else s"""$$('$s')""" + (html.children.map { child =>
      val s = JavaScript.expression(child)
      if (s.isEmpty) "" else s"""\n.append($s)"""
    }).mkString

    val events = Event.eventsOf(html.node)
    events.foldLeft(s2) { (memo, event) =>
      s"${memo}.on('${event.name}', ${event.methodName})"
    }
  }
  def expression(html: HtmlComponent): String = {
    s"$$(new ${html.className}().$$el)" //TODO args
  }
  def clazz(htmlComponent: HtmlComponent): String = {
    val div = HtmlNode(<div/>, htmlComponent.template.children)
    val s = expression(div)
    val events = allEvents(htmlComponent.template)
    /*
    val eventFunctions = events.map { e =>
      s"""${e.methodName}: function(){
      alert("${e.methodName}")
    }"""
    }.mkString(",")
*/
    val eventFunctions = htmlComponent.script
    s"""
	Backbone.View.extend({
	  initialize: function(){
        ${eventFunctions}
	    this.$$el = $$($s);
	  }
	})"""
  }
  def clazzDef(htmlComponent: HtmlComponent): String = {
    s"var ${htmlComponent.className} = ${clazz(htmlComponent)};"
  }
  def allEvents(htmlNodes: Seq[HtmlNode]): Seq[Event] = {
    htmlNodes.flatMap(allEvents)
  }
  def allEvents(html: HtmlNode): Iterable[Event] = {
    for {
      event <- (Event.eventsOf(html.node) ++ allEvents(html.children)).seq
    } yield event
  }

}