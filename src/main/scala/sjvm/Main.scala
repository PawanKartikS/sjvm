package sjvm

object Main {
  def main(args: Array[String]): Unit = {
    new sjvm.runtime.VirtualMachine(new CmdlineArguments(args)).run()
  }
}
