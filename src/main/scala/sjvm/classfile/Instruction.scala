package sjvm.classfile

import scala.collection.mutable

object Opcode {
  val CLASS_CONSTRUCTOR = "invokespecial"
  val CLASS_METHOD = "invokevirtual"
  val STATIC_METHOD = "invokestatic"
  val END_OF_METHOD = "return"
}

class Instruction(val index: Int, val opcode: String) {
  private final val operands = mutable.ArrayBuffer[Any]()

  def addOperand(operand: Any): Unit = {
    if (operand != null)
      operands += operand
  }

  def getOperand[A](n: Int): A = {
    val v = operands(n)

    v match {
      case _: Int | _: Long | _: Float | _: Double | _: String => v.asInstanceOf[A]
      case _ => throw new IllegalArgumentException(s"unexpected type for operand at $n")
    }
  }

  def isEndOfMethod: Boolean = {
    opcode.equals(Opcode.END_OF_METHOD)
  }

  def isFunctionCall: Boolean = {
    opcode match {
      case Opcode.CLASS_CONSTRUCTOR | Opcode.CLASS_METHOD | Opcode.STATIC_METHOD => true
      case _ => false
    }
  }

  def operandsCount: Int = {
    operands.length
  }
}
