package org.fermat

import org.fermat.util.Tsort
import org.fermat.util.Tsort.{ Node => TNode }

object Dependency {
  
  def getAllComponent(jarPath: String, top: Component, fullPathOf: String => String): List[Component] = {
    val getComponent: String => Component = (src: String) => {
      Component(jarPath, fullPathOf(src)).right.get
    }
    val (c, all) = Dependency.makeNodesForTsort(top, Map(), getComponent, Nil)
    Tsort.exec(all).reverse
  }
  
  private def makeNodesForTsort(top: Component, cache: Map[String, Component], getComponent: String => Component,
    tNodeList: List[TNode[Component]]): (Map[String, Component], List[TNode[Component]]) = {
    val (newCache, reqComponents) = top.requires.foldLeft((cache, List[Component]())) {
      case ((memo, list), Require(src)) =>
        memo.get(src) match {
          case Some(component) => {
            (memo, component :: list)
          }
          case None => {
            val component = getComponent(src)
            (memo + (src -> component), component :: list)
          }
        }
    }
    val init = (newCache, TNode(top, reqComponents) :: tNodeList)
    reqComponents.foldLeft(init) {
      case ((cache, list), reqComponent) =>
        makeNodesForTsort(reqComponent, cache, getComponent, list)
    }
  }
}