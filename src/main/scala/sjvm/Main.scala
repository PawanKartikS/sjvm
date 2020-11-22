package sjvm

import sjvm.classfile.{JClass, Parser}
import sjvm.runtime.VirtualMachine

import scala.collection.mutable

object Main {
  def main(args: Array[String]): Unit = {
    val classes = mutable.HashMap[String, JClass]()
    val cmdlineArguments = new CmdlineArguments(args)

    val parser = new Parser(cmdlineArguments)
    for (classFile <- cmdlineArguments.getClassFiles) {
      val jclass = parser.parse(classFile)
      classes.put(jclass.name, jclass)
    }

    val virtualMachine = new VirtualMachine(cmdlineArguments, classes)
    virtualMachine.run()
  }
}