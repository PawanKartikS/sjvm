package sjvm

import scala.collection.mutable

class CmdlineArguments(args: Array[String]) {
  var mainClass: String = _
  var rtPath: String = _
  var opcodeVerbose = false
  var showClassFields = false
  private val classFiles = mutable.ArrayBuffer[String]()

  args.foreach(collectArg)

  if (mainClass == null)
    throw new IllegalArgumentException("cannot possibly init jvm, missing main class")

  if (rtPath == null)
    println("cannot possibly fully init jvm, missing rt path")

  private def collectArg(arg: String): Unit = {
    if (isAClassFile(arg)) {
      classFiles += arg
    } else if (isASwitch(arg)) {
      parseArgument(arg)
    } else {
      println(s"skipping unknown arg - $arg")
    }
  }

  def getClassFiles: mutable.ArrayBuffer[String] = {
    classFiles
  }

  private def isAClassFile(arg: String): Boolean = {
    val idx = arg.length - 6
    arg.length >= 7 && arg.substring(idx).equals(".class")
  }

  private def isASwitch(arg: String): Boolean = {
    arg(0) == '-'
  }

  def parseArgument(arg: String): Unit = {
    arg match {
      case "-jvm:OpcodeVerbose" => opcodeVerbose = true
      case "-jvm:ShowClassFields" => showClassFields = true

      case _ =>
        if (arg.contains("-jvm:MainClass=")) {
          mainClass = arg.substring(arg.indexOf('=') + 1)
        } else if (arg.contains("-jvm:RT=")) {
          rtPath = arg.substring(arg.indexOf('=') + 1)
        } else {
          println(s"cannot recognize $arg, skipping")
        }
    }
  }
}