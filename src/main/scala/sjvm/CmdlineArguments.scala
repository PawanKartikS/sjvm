package sjvm

import scala.collection.mutable

class CmdlineArguments(args: Array[String]) {
  var mainClass = "Main"
  var rtPath: String = _
  var classpath: String = _

  // cmd line args
  var parseVerbose = false
  var opcodeVerbose = false
  var showClassFields = false
  var showUnusedClasses = false
  var showUnusedVarCount = false
  
  private val classFiles = mutable.ArrayBuffer[String]()

  args.foreach(collectArg)

  if (rtPath == null)
    println("cannot possibly fully init jvm, missing rt path")

  if (classpath == null || classpath.isEmpty)
      throw new IllegalArgumentException("classpath is unspecified")

  private def collectArg(arg: String): Unit = {
    if (isASwitch(arg)) {
      parseArgument(arg)
    } else {
      println(s"skipping unknown arg - $arg")
    }
  }

  def getClassFiles: mutable.ArrayBuffer[String] = {
    classFiles
  }

  private def isASwitch(arg: String): Boolean = {
    arg(0) == '-'
  }

  def parseArgument(arg: String): Unit = {
    arg match {
      case "-jvm:ShowAllWarns" =>
        parseVerbose = true
        opcodeVerbose = true
        showClassFields = true
        showUnusedClasses = true
        showUnusedVarCount = true

      case "-jvm:ParseVerbose" => parseVerbose = true
      case "-jvm:OpcodeVerbose" => opcodeVerbose = true
      case "-jvm:ShowClassFields" => showClassFields = true
      case "-jvm:ShowUnusedClasses" => showUnusedClasses = true
      case "-jvm:ShowUnusedVarCount" => showUnusedVarCount = true

      case _ =>
        if (arg.contains("-jvm:MainClass=")) {
          mainClass = arg.substring(arg.indexOf('=') + 1)
        } else if (arg.contains("-jvm:RT=")) {
          rtPath = arg.substring(arg.indexOf('=') + 1)
        } else if (arg.contains("-jvm:classpath=")) {
          classpath = arg.substring(15)
        } else {
          println(s"cannot recognize $arg, skipping")
        }
    }
  }
}
