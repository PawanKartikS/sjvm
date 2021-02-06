package sjvm.classfile

import com.sun.org.apache.bcel.internal.classfile.Method
import sjvm.CmdlineArguments

import scala.util.control.Breaks.{break, breakable}

object Parser {
  private final val TOKENIZE_REGEX = "\\s+"
  private final val METHOD_SKIP_PARSE = "max_stack"
  private final val METHOD_STOP_PARSE = "Attribute(s)"

  private def parseArgument(opcode: String, token: String): Any = {
    opcode match {
      case "l2d" |
           "l2f" |
           "l2i" |
           "ladd" |
           "laload" |
           "land" |
           "lastore" |
           "lcmp" |
           "lconst" |
           "ldiv" |
           "lload" |
           "lmul" |
           "lneg" |
           "lor" |
           "lrem" |
           "lreturn" |
           "lshl" |
           "lshr" |
           "lstore" |
           "lsub" |
           "lushr" |
           "lxor" =>
        token.toLong

      case "f2d" |
           "f2i" |
           "f2l" |
           "fadd" |
           "faload" |
           "fastore" |
           "fcmpg" |
           "fcmpl" |
           "fconst" |
           "fdiv" |
           "fload" |
           "fmul" |
           "fneg" |
           "frem" |
           "freturn" |
           "fstore" |
           "fsub" =>
        token.toFloat

      case "dadd" |
           "daload" |
           "dastore" |
           "dcmpg" |
           "dcmpl" |
           "dconst" |
           "ddiv" |
           "dload" |
           "dmul" |
           "dneg" |
           "drem" |
           "dreturn" |
           "dstore" |
           "dsub" =>
        token.toDouble

      case _ => token.toInt
    }
  }

  private def parseInstruction(line: String): Instruction = {
    val parseOffset = (token: String) => {
      token.substring(0, token.length - 1).toInt
    }

    val parseOpcode = (token: String) => {
      val pos = token.indexOf('_')

      if (pos == -1 || !token(pos + 1).isDigit)
        (token, null)
      else {
        val opcode = token.substring(0, pos)
        // Part after _ is guaranteed to be Int.
        (opcode, parseArgument(opcode, token.substring(pos + 1)))
      }
    }

    var index = 0
    var instruction: Instruction = null
    val tokens = line.split(TOKENIZE_REGEX).filter(t => t.nonEmpty)

    breakable {
      for (i <- tokens.indices) {
        if (tokens(i)(0) == '/')
          break()

        i match {
          case 0 => index = parseOffset(tokens(i))
          case 1 =>
            val (opcode, operand) = parseOpcode(tokens(i))
            instruction = new Instruction(index, opcode)
            instruction.addOperand(operand)

          case _ =>
            var operand = tokens(i)

            // Types of operands -
            // #Some_Operand - trim first char.
            // Some_Int, - trailing comma; for ex - iinc 1, 1.
            // (Some_Operand) - trim first and last char.
            // Some_Operand - process as a string.
            if (operand(0) == '#' || operand(0) == '%') {
              operand = operand.substring(1)
            } else if (operand.last == ',') {
              operand = operand.substring(0, operand.length - 1)
            } else if (operand(0) == '(' && operand.last == ')') {
              operand = operand.substring(1, operand.length - 1)
            }

            // We can do better here!
            try {
              instruction.addOperand(parseArgument(instruction.opcode, operand))
            } catch {
              case _: NumberFormatException => instruction.addOperand(operand)
            }
        }
      }
    }

    instruction
  }

  private def parseMethod(method: Method): JMethod = {
    val lookupSignature = s"${method.getName}${method.getSignature}"
    val jmethod = new JMethod(lookupSignature)

    val body = method.getCode.toString
    val instructions = body.split("\n")
      .filter(line => line.nonEmpty && !line.contains(METHOD_SKIP_PARSE))

    var i = 0
    for (instruction <- instructions) {
      if (instruction.contains(METHOD_STOP_PARSE))
        return jmethod

      val instr = parseInstruction(instruction)
      jmethod.addInstruction(instr)
      jmethod.jmpOffset.put(instr.index, i)

      i += 1
    }

    throw new IllegalArgumentException(s"invalid class file - expecting attribute(s)")
  }

  def parse(classFile: String): JClass = {
    val pclass = Decompiler.decompile(classFile)
    val jclass = new JClass(pclass.getClassName)

    jclass.pool(pclass.getConstantPool)
    jclass.setFields(pclass.getFields)

    val methods = pclass.getMethods

    for (method <- methods) {
      val jmethod = parseMethod(method)
      jclass.newMethod(jmethod.name, jmethod)
    }

    jclass
  }
}

