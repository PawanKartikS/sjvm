package sjvm.classfile

import com.sun.org.apache.bcel.internal.classfile.{ClassParser, JavaClass}

object Decompiler {
  def decompile(classFilePath: String): JavaClass = {
    new ClassParser(classFilePath).parse()
  }
}
