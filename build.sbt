import AssemblyKeys._ // put this at the top of the file
import ProjectBuild._

assemblySettings

name := "fermat"

version := "0.1.0"

scalaVersion := "2.10.3"

mainClass in assembly := Some("org.fermat.Main")

val all = inputKey[Unit]("all")

all := {
    val a = assembly.value
    val b = copy.value
    ()
}