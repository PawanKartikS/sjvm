package sjvm.classfile

import scala.collection.mutable

class JMethod(val name: String) {
  final val jmpOffset = mutable.HashMap[Int, Int]()

  private final val instructions = mutable.ArrayBuffer[Instruction]()

  def addInstruction(instruction: Instruction): Unit = {
    instructions += instruction
  }

  def getInstructions: mutable.ArrayBuffer[Instruction] = {
    instructions
  }
}