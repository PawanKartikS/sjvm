package sjvm

import sjvm.classfile.JClass
import sjvm.runtime.VirtualMachine

import scala.collection.mutable

object Main {
  def main(args: Array[String]): Unit = {
    val classes = mutable.HashMap[String, JClass]()
    val cmdlineArguments = new CmdlineArguments(args)
    val virtualMachine = new VirtualMachine(cmdlineArguments)
    virtualMachine.run()
  }
}