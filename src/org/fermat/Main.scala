package org.fermat
import scala.xml.XML
import org.fermat.web.Web
import org.fermat.web.Html

object Main {
  def main(args: Array[String]) = {

    exec("sandbox", "component1.frm")
  }
  
  def exec(root: String, top: String) {
    val component1Xml = Dao.loadXml(root + "/" + top);
    val component1 = Component(component1Xml)
    		
    Dao.write(Web.all(component1))
  }
}








