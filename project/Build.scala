import sbt._

object ProjectBuild extends Build {
  //  memo: assembly => copy => install
  
  def copy(src: String, dest: String) = {
    import java.io.{ File, FileInputStream, FileOutputStream }
    val srcFile = new File(src)//TODO
    val destFile = new File(dest)
    new FileOutputStream(destFile).getChannel().transferFrom(
      new FileInputStream(srcFile).getChannel(), 0, Long.MaxValue)
  } 
  
  val allKey = TaskKey[Unit]("all", "all task")
  val all = allKey := {
    copy("src/fermat", "dest/fermat")
    copy("src/fermat.xsd", "dest/fermat.xsd")
    copy("target/scala-2.10/fermat-assembly-0.1.0.jar", "dest/fermat.jar")//TODO
  }

  lazy val root = Project(
    id = "all_task",
    base = file("."),
    settings = Defaults.defaultSettings ++ Seq(all))
}