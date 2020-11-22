package sjvm.classfile

import com.sun.org.apache.bcel.internal.classfile.{ConstantPool, Field}

import scala.collection.mutable

class JClass(val name: String) {
  var cpool: ConstantPool = _

  private final val fields = mutable.HashMap[String, Field]()
  private final val methods = mutable.HashMap[String, JMethod]()

  def pool(pool: ConstantPool): Unit = {
    cpool = pool
  }

  def getFields: mutable.HashMap[String, Field] = {
    fields
  }

  def getMethods: mutable.HashMap[String, JMethod] = {
    methods
  }

  def getMethod(name: String): JMethod = {
    val hmval = methods.get(name)

    hmval match {
      case Some(_) => hmval.get
      case _ => null
    }
  }

  def setFields(fields: Array[Field]): Unit = {
    for (field <- fields) {
      this.fields.put(field.getName, field)
    }
  }

  def newMethod(name: String, method: JMethod): Unit = {
    methods.put(name, method)
  }
}