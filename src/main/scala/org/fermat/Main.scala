package org.fermat
import scala.xml.XML
import org.fermat.web.Web
import org.fermat.web.Html
import org.fermat.util.Dao

object Main {

  def main(args: Array[String]) = {
    println("start!")
    val root = args(0)
    val top = args(1)
    println(s"root: $root top:$top")

    exec(root, top)
    println("end!")
  }

  def exec(root: String, top: String) {

    val fullPathOf = (s: String) => root + "/" + s
    val fullPathOfTop = fullPathOf(top)

    Component(fullPathOfTop) match {
      case Right(topComponent) => {
        val deps = Dependency.getAllComponent(topComponent, fullPathOf)
        Dao.write(Web.all(deps))
      }
      case Left(ex) => ex.cause.printStackTrace()
    }
  }
}