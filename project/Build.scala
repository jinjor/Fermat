import sbt._

object ProjectBuild extends Build {
  //  memo: assembly => copy => install

  def copyFile(src: String, dest: String) = {
    import java.io.{ File, FileInputStream, FileOutputStream }
    val srcFile = new File(src) //TODO
    val destFile = new File(dest)
    new FileOutputStream(destFile).getChannel().transferFrom(
      new FileInputStream(srcFile).getChannel(), 0, Long.MaxValue)
  }
  val copy = TaskKey[Unit]("copy", "copy task")
  
  lazy val root = Project(
    id = "copy_task",
    base = file("."),
    settings = Defaults.defaultSettings ++ Seq(copy := {
      copyFile("src/main/resources/fermat", "dest/fermat")
      copyFile("src/main/resources/fermat.xsd", "dest/fermat.xsd")
      copyFile("target/scala-2.10/fermat-assembly-0.1.0.jar", "dest/fermat.jar") //TODO
    }))
}