package sjvm.runtime

import com.sun.org.apache.bcel.internal.classfile.Field

import scala.collection.mutable

object WrapperClass {
  val OBJECT = "java.lang.Object.<init>"
  val DOUBLE = "<java.lang.Double>"
  val FLOAT = "<java.lang.Float>"
  val INTEGER = "<java.lang.Integer>"
  val STRING = "<java.lang.String>"
}

class Instance(val name: String, classFields: mutable.HashMap[String, Field]) {
  private final val fields = mutable.HashMap[String, (String, Any)]()

  if (classFields != null)
    classFields.foreach(f => fields.put(f._2.getName, (f._2.getType.getSignature, null)))

  def getfield(field: String): Any = {
    val hmval = fields.get(field)
    hmval match {
      case Some(_) => hmval.get._2
      case _ => null
    }
  }

  def getfields: mutable.HashMap[String, (String, Any)] = {
    fields
  }

  def init(): Unit = {
    for (field <- fields) {
      val key = field._1
      val ty = field._2._1

      fields.update(key, (ty, ty match {
        case "I" => 0
        case "D" => 0.0
        case "Ljava/lang/String;" => ""

        // Temp
        case _ => throw new IllegalArgumentException(s"ty not supported $ty")
      }))
    }
  }

  def putfield(field: String, newValue: Any): Unit = {
    val hmval = fields.get(field)
    val value = hmval match {
      case Some(_) => hmval.get
      case _ => throw new IllegalArgumentException(s"no such field exists $field")
    }

    fields.update(field, (value._1, newValue))
  }
}