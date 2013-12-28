package org.fermat

import scala.annotation.tailrec

object Tsort {
  case class NodeNotFound[A](parent: A) extends Exception
  case class Node[A](value : A, parents: List[A])
  
  def notVisited[A](visited: Set[A], name:A):Boolean = !visited.contains(name)

  def visit[A](node: Node[A], visited: Set[A], l: List[A], getNode:A => Option[Node[A]]): (Set[A], List[A]) = {
    val Node(value, parents) = node
    if(notVisited(visited, value)){
      val visited_ = visited + value
      val (visited__, l_) = parents.foldLeft((visited_, l)){ (memo, parent) => {
        val (visited:Set[A], l: List[A]) = memo
        getNode(parent) match {
          case Some(n) => visit(n, visited, l, getNode)
          case None => throw NodeNotFound(parent)
        }
      }
      }
      (visited__, (value :: l_))
    }else{
      (visited, l)
    }
  }
  
  def exec[A](all: List[Node[A]]): List[A] = {
    val getNode = (value: A) => {
      all.find{a => a.value == value}
    }
    val (visited, l) = all.foldLeft((Set[A](), List[A]())) { (memo, n) =>
       val (visited, l) = memo
       visit(n, visited, l, getNode)
    }
    l
  }
  
  def main(args: Array[String]) {
	  val node1 = Node(1, Nil)
	  val node2 = Node(2, List(1))
	  val node3 = Node(3, List(1, 4))
	  val node4 = Node(4, List(2))
	  
	  println(Tsort.exec[Int](List(node1, node2, node3, node4)))
	  
  }
}
