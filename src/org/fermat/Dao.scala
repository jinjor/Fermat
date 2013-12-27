package org.fermat

import org.apache.commons.io.FileUtils
import java.io.File
import scala.xml.XML
import scala.xml.Elem

object Dao {
	def load: String => String = fileName => {
		FileUtils.readFileToString( new File(fileName) )
	}
	def loadXml: String => Elem = fileName => {
		XML.loadString(load(fileName))
	}
	def write(output: Output) {
	   write(output.filePath, output.content)
	}
	def write(filePath: String, content: String) {
	  FileUtils.write(new File(filePath), content)
	}
}

case class Output(filePath:String, content:String)