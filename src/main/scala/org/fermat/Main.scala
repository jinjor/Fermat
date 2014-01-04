package org.fermat
import scala.xml.XML
import org.fermat.web.Web
import org.fermat.web.Html
import org.fermat.util.Dao

object Main {

  def main(args: Array[String]) = {
    println("start!")
    val jarPath = args(0)
    val root = args(1)
    val top = args(2)
    println(s"root: $root top:$top")

    exec(jarPath, root, top)
    println("end!")
  }

  private def exec(jarPath:String, root: String, top: String) {

    val fullPathOf = (s: String) => root + "/" + s
    val fullPathOfTop = fullPathOf(top)

    Component(jarPath, fullPathOfTop) match {
      case Right(topComponent) => {
        val deps = Dependency.getAllComponent(jarPath, topComponent, fullPathOf)
        Dao.write(Web.all(root, deps))
      }
      case Left(ex) => ex.cause.printStackTrace()
    }
  }
}