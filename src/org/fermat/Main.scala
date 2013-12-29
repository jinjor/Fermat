package org.fermat
import scala.xml.XML
import org.fermat.web.Web
import org.fermat.web.Html

object Main {
  def main(args: Array[String]) = {

    exec("sandbox", "component1.xml")
  }
  
  def exec(root: String, top: String) {
    val path = root + "/" + top
    
    val component1 = Component(path) match {
      case Right(component) => Dao.write(Web.all(component))
      case Left(ex) => ex.cause.printStackTrace()
    }
    
  }
}








