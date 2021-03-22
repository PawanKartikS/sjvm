package sjvm.runtime

import com.sun.org.apache.bcel.internal.classfile.ConstantPool
import sjvm.CmdlineArguments
import sjvm.classfile.{Instruction, JClass, Parser, Opcode}

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

class VirtualMachine(cmdlineArguments: CmdlineArguments) {
  private final val DEFAULT_ARR_SIZE = 128
  private final val PSVM = "main([Ljava/lang/String;)V"

  class Frame {
    final val accessCount = mutable.HashMap[Int, Int]()
    final var localvar = new Array[Any](DEFAULT_ARR_SIZE)
    final val stack = mutable.Stack[Any]()

    def this(args: Array[Any], isStaticContext: Boolean) = {
      this()
      if (args.length > DEFAULT_ARR_SIZE)
        localvar = new Array[Any](DEFAULT_ARR_SIZE + args.length)

      val adj = if (isStaticContext) 0 else 1
      for (i <- args.indices) {
        localvar(i + adj) = args(i)
      }
    }
  }

  private var frame: Frame = _
  private var jmpOffset: mutable.HashMap[Int, Int] = _

  private val classes = mutable.HashMap[String, JClass]()
  private val classFiles = cmdlineArguments.getClassFiles
  private val frames = mutable.Stack[Frame]()
  private val instances = mutable.HashMap[String, Instance]()

  private def incrementBy(n: Int, by: Int): Unit = {
    if (n >= frame.localvar.length)
      throw new IllegalArgumentException("n exceeds buffer capacity")

    val oldval = frame.localvar(n).asInstanceOf[Int]
    val newval = oldval + by

    frame.localvar(n) = newval
  }

  private def dup(n: Int): Unit = {
    n match {
      case 1 => push(peek())
      case 2 =>
        val b = pop()
        val a = pop()

        push(a)
        push(b)
        push(a)
        push(b)

      case _ => // handle?
    }
  }

  private def getCpool(className: String): ConstantPool = {
    val hmval = classes.get(className)

    hmval match {
      case Some(_) => hmval.get.cpool
      case _ => throw new NoClassDefFoundError(s"cannot find class $className")
    }
  }

  private def invokerFrame: Frame = {
    if (frames.length <= 1)
      throw new IllegalArgumentException("no invoker frame available for current context")

    frames(frames.length - 2)
  }

  private def isInstanceOf(obj: Any, ty: String): Boolean = {
    ty.equals(obj match {
      case _: Int => WrapperClass.INTEGER
      case _: Float => WrapperClass.FLOAT
      case _: Double => WrapperClass.DOUBLE
      case _: String => WrapperClass.STRING
      case _ => throw new IllegalArgumentException(s"could not deduce type for $obj")
    })
  }

  private def math1(opcode: String): Boolean = {
    val x = pop().asInstanceOf[Int]

    opcode match {
      case "ifeq" => x == 0
      case "ifne" => x != 0
      case "iflt" => x < 0
      case "ifle" => x <= 0
      case "ifgt" => x > 0
      case "ifge" => x >= 0

      case "ineg" =>
        push(x * -1)
        false

      case _ => throw new IllegalArgumentException(s"invalid opcode $opcode")
    }
  }

  private def math2(opcode: String): Boolean = {
    val x1 = pop().asInstanceOf[Int]
    val x2 = pop().asInstanceOf[Int]

    opcode match {
      case "iadd" =>
        push(x1 + x2)
        false

      case "idiv" =>
        push(x2 / x1)
        false

      case "imul" =>
        push(x1 * x2)
        false

      case "isub" =>
        push(x2 - x1)
        false

      case "if_icmpeq" => x1 == x2
      case "if_icmpne" => x1 != x2
      case "if_icmplt" => x2 < x1
      case "if_icmple" => x2 <= x1
      case "if_icmpgt" => x2 > x1
      case "if_icmpge" => x2 >= x1

      case "iand" =>
        push(x1 & x2)
        false

      case "ior" =>
        push(x1 | x2)
        false

      case "ixor" =>
        push(x1 ^ x2)
        false

      case _ => throw new IllegalArgumentException(s"invalid opcode $opcode")
    }
  }

  private def nullCheck(mustBeNull: Boolean, i: Int, n: Int): Int = {
    val x = pop()

    // if condition fails, go to the next instruction
    if (!(if (mustBeNull) x == null else x != null))
      return i + 1

    val jmp = jmpOffset.get(n)
    jmp match {
      // jump to the offset required
      case Some(_) => jmp.get
      case _ => throw new IllegalArgumentException(s"cannot look up jump offset for $n")
    }
  }

  private def push(element: Any): Unit = {
    frame.stack.push(element)
  }

  private def peek(): Any = {
    if (frame.stack.isEmpty)
      throw new IllegalArgumentException("cannot peek, stack is empty")

    frame.stack.top
  }

  private def pop(): Any = {
    if (frame.stack.isEmpty)
      throw new IllegalArgumentException("cannot pop, stack is empty")

    frame.stack.pop()
  }

  private def readyClass(className: String): Boolean = {
    classes.get(className) match {
      case Some(_) => return true
      case _ =>
        for (classFile <- classFiles) {
          if (classFile.contains(s"$className.class")) {
            var jclass = Parser.parse(classFile, cmdlineArguments.parseVerbose)
            classes.put(jclass.name, jclass)
            return true
          }
        }
    }

    false
  }

  private def resolveToNameType(cpstring: String): (String, String) = {
    val idx = cpstring.indexOf('.')
    if (idx == -1)
      throw new IllegalArgumentException(s"invalid entry - $cpstring")

    (cpstring.substring(0, idx), cpstring.substring(idx + 1))
  }

  private def swap(): Unit = {
    val b = pop()
    val a = pop()

    push(b)
    push(a)
  }

  private def toArray(n: Int): Unit = {
    if (n >= frame.localvar.length)
      throw new IllegalArgumentException("n exceeds buffer capacity")

    frame.accessCount.get(n) match {
      case Some(_) => frame.accessCount.update(n, frame.accessCount(n) + 1)
      case _ => frame.accessCount.put(n, 1)
    }

    frame.localvar(n) = pop()
  }

  private def toStack(n: Int): Unit = {
    if (n >= frame.localvar.length)
      throw new IllegalArgumentException("n exceeds buffer capacity")

    frame.accessCount.get(n) match {
      case Some(_) => frame.accessCount.update(n, frame.accessCount(n) + 1)
      case _ => frame.accessCount.put(n, 1)
    }

    push(frame.localvar(n))
  }

  private def eval(className: String, instruction: Instruction, i: Int): Int = {
    if (cmdlineArguments.opcodeVerbose)
      println(s"${instruction.index} $className ${instruction.opcode}")

    instruction.opcode match {
      case "aaload" |
           "baload" |
           "caload" |
           "daload" |
           "faload" |
           "iaload" |
           "laload" |
           "saload" =>
        val pos = pop().asInstanceOf[Int]
        val arr = pop().asInstanceOf[Array[Any]]

        push(arr(pos))

      case "aconst_null" =>
        push(null)

      case "aload" =>
        val n = instruction.getOperand[Int](0)
        if (n == 0) push(instances(className)) else toStack(n)

      case "anewarray" |
           "newarray" =>
        val size = pop().asInstanceOf[Int]
        push(new Array[Any](size))

      case "arraylength" =>
        val arr = pop().asInstanceOf[Array[Any]]
        push(arr.length)

      case "astore" |
           "dstore" |
           "fstore" |
           "istore" |
           "lstore" =>
        val n = instruction.operandsCount match {
          case 0 => pop().asInstanceOf[Int]
          case _ => instruction.getOperand[Int](0)
        }

        toArray(n)

      case "aastore" |
           "bastore" |
           "castore" |
           "daload" |
           "fastore" |
           "iastore" |
           "lastore" |
           "sastore" =>
        val arv = pop()
        val pos = pop().asInstanceOf[Int]
        val arr = pop().asInstanceOf[Array[Any]]

        arr(pos) = arv

      case "bipush" |
           "sipush" =>
        val value = instruction.getOperand[Int](0)
        push(value)

      case "checkcast" =>
        val cast = instruction.getOperand[String](0)
        val obj = pop()

        if (!isInstanceOf(obj, cast)) {
          throw new Exception(s"cast error - object $obj is not of type $cast")
        }

      case "d2f" =>
        val n = pop().asInstanceOf[Double]
        push(n.toFloat)

      case "d2i" =>
        val n = pop().asInstanceOf[Double]
        push(n.toInt)

      case "d2l" =>
        val n = pop().asInstanceOf[Double]
        push(n.toLong)

      case "dconst" =>
        push(instruction.getOperand[Double](0))

      case "drem" =>
        val x1 = pop().asInstanceOf[Double]
        val x2 = pop().asInstanceOf[Double]
        push(x2 % x1)

      case "areturn" |
           "dreturn" |
           "freturn" |
           "ireturn" |
           "lreturn" =>
        invokerFrame.stack.push(pop())

      case "dup" =>
        dup(1)

      case "dup2" =>
        dup(2)

      case "f2d" =>
        val n = pop().asInstanceOf[Float]
        push(n.toDouble)

      case "f2i" =>
        val n = pop().asInstanceOf[Float]
        push(n.toInt)

      case "f2l" =>
        val n = pop().asInstanceOf[Float]
        push(n.toLong)

      case "fconst" =>
        push(instruction.getOperand[Float](0))

      case "frem" =>
        val x1 = pop().asInstanceOf[Float]
        val x2 = pop().asInstanceOf[Float]
        push(x2 % x1)

      case "getfield" =>
        val (_, field) = resolveToNameType(instruction.getOperand[String](0))
        val instance = instances(className)

        push(instance.getfield(field))

      case "goto" =>
        val n = instruction.getOperand[Int](0)
        val jmp = jmpOffset.get(n)

        jmp match {
          // jump to the offset required
          case Some(_) => return jmp.get
          case _ => throw new RuntimeException(s"cannot look up jump offset for $n")
        }

      case "iconst" =>
        push(instruction.getOperand[Int](0))

      case "iconst_m1" =>
        push(-1)

      case "iadd" |
           "idiv" |
           "imul" |
           "isub" |
           "if_icmpeq" |
           "if_icmpne" |
           "if_icmplt" |
           "if_icmple" |
           "if_icmpgt" |
           "if_icmpge" |
           "iand" |
           "ior" |
           "ixor" =>
        // Jump only if condition is satisfied
        if (math2(instruction.opcode)) {
          val n = instruction.getOperand[Int](0)
          val jmp = jmpOffset.get(n)

          jmp match {
            case Some(_) => return jmp.get
            case _ => throw new RuntimeException(s"cannot look up jump offset for $n")
          }
        }

      case "ifeq" |
           "ifne" |
           "iflt" |
           "ifle" |
           "ifgt" |
           "ifge" |
           "ineg" =>
        // Jump only if condition is satisfied
        if (math1(instruction.opcode)) {
          val n = instruction.getOperand[Int](0)
          val jmp = jmpOffset.get(n)

          jmp match {
            case Some(_) => return jmp.get
            case _ => throw new RuntimeException(s"cannot look up jump offset for $n")
          }
        }

      case "ifnull" =>
        val n = instruction.getOperand[Int](0)
        return nullCheck(mustBeNull = true, i, n)

      case "ifnonnull" =>
        val n = instruction.getOperand[Int](0)
        return nullCheck(mustBeNull = false, i, n)

      case "iinc" =>
        val n = instruction.getOperand[Int](0)
        val by = instruction.getOperand[Int](1)

        incrementBy(n, by)

      case "iload" =>
        val n = instruction.getOperand[Int](0)
        toStack(n)

      case "instanceof" =>
        val obj = instruction.getOperand[String](0)
        val tos = pop()
        push(if (isInstanceOf(tos, obj)) 1 else 0)

      case "irem" =>
        val x1 = pop().asInstanceOf[Int]
        val x2 = pop().asInstanceOf[Int]
        push(x2 % x1)

      case "ishl" =>
        val shiftCount = pop().asInstanceOf[Int]
        val n = pop().asInstanceOf[Int]
        push(n << shiftCount)

      case "ishr" =>
        val shiftCount = pop().asInstanceOf[Int]
        val n = pop().asInstanceOf[Int]
        push(n >> shiftCount)

      case "ldc" =>
        push(instruction.getOperand[String](0))

      case "ldc2_w" =>
        push(instruction.getOperand[Long](0))

      case "lshl" =>
        val shiftCount = pop().asInstanceOf[Int]
        val n = pop().asInstanceOf[Long]
        push(n << shiftCount)

      case "lshr" =>
        val shiftCount = pop().asInstanceOf[Int]
        val n = pop().asInstanceOf[Long]
        push(n >> shiftCount)

      case "new" =>
        val index = instruction.getOperand[Int](1)
        val cpool = getCpool(className)

        val instname = cpool.constantToString(cpool.getConstant(index))
        if (!readyClass(instname))
          throw new RuntimeException(s"could not lazy load class - $instname")

        val instance = new Instance(instname, classes(instname).getFields)
        instance.init()

        push(instance)
        instances.put(instname, instance)

      case "nop" =>
      case "pop" =>
        pop()

      case "putfield" =>
        val (_, field) = resolveToNameType(instruction.getOperand[String](0))

        val fieldval = pop()
        val instance = pop().asInstanceOf[Instance]
        instance.putfield(field, fieldval)

      case "swap" =>
        swap()

      case _ => throw new IllegalArgumentException(s"unsupported opcode - ${instruction.opcode}")
    }

    // go to the next instruction
    i + 1
  }

  private def evalMethod(className: String, methodName: String): Unit = {
    if (!readyClass(className))
      throw new RuntimeException(s"could not lazy load class - $className")

    val jmethod = classes(className).getMethod(methodName)
    if (jmethod == null)
      throw new IllegalArgumentException(s"cannot find method $methodName")

    var i = 0
    val instructions = jmethod.getInstructions

    jmpOffset = jmethod.jmpOffset

    breakable {
      while (i < instructions.length) {
        val instr = instructions(i)
        if (instr.isEndOfMethod)
          break()

        if (instr.isFunctionCall) {
          invokeMethod(instr)
          i += 1
        } else {
          // Do not increment i.
          // It already moved to i + 1 or to the required offset (jump)
          // by eval().
          i = eval(className, instr, i)
        }
      }
    }

    if (cmdlineArguments.showUnusedVarCount) {
      val unusedVars = frame.accessCount.filter(p => p._2 == 1).size
      if (unusedVars >= 1)
        println(s"$unusedVars unused var in $methodName")
    }
    
    frames.pop()
    if (frames.nonEmpty)
      frame = frames.top
  }

  private def invokeMethod(instruction: Instruction): Unit = {
    val inv = instruction.getOperand[String](0)
    if (inv.equals(WrapperClass.OBJECT)) {
      return
    }

    val (instance, method) = resolveToNameType(inv)
    val ty = instruction.getOperand[String](1)

    unpackArguments(ty, method.equals(Opcode.STATIC_METHOD))
    evalMethod(instance, method + ty)
  }

  def run(): Unit = {
    frames.push(new Frame())
    frame = frames.top
    evalMethod(cmdlineArguments.mainClass, PSVM)

    println()

    if (cmdlineArguments.showUnusedClasses) {
      if (classes.size != classFiles.length)
        println("jvm: Unused classes detected")
    }

    if (!cmdlineArguments.showClassFields)
      return

    for (instance <- instances) {
      val fields = instance._2.getfields
      for (f <- fields) {
        println(s"${f._1} -> ${f._2._2}")
      }
    }
  }

  private def unpackArguments(signature: String, isStaticContext: Boolean): Unit = {
    var arglen = 0

    // TODO: Remember to update this to support different arg types!
    for (c <- signature) {
      arglen += (c match {
        case ';' | 'I' | 'D' => 1
        case _ => 0
      })
    }

    if (arglen == 0) {
      frames.push(new Frame())
      frame = frames.top
      return
    }

    val args = new Array[Any](arglen)
    for (i <- arglen - 1 to(0, -1)) {
      args(i) = pop()
    }

    frames.push(new Frame(args, isStaticContext))
    frame = frames.top
  }
}
