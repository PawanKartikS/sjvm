New-Item -ItemType Directory -ErrorAction SilentlyContinue "out"

scalac `
    src/main/scala/sjvm/classfile/Decompiler.scala `
    src/main/scala/sjvm/classfile/Instruction.scala `
    src/main/scala/sjvm/classfile/JClass.scala `
    src/main/scala/sjvm/classfile/JMethod.scala `
    src/main/scala/sjvm/classfile/Parser.scala `
    src/main/scala/sjvm/runtime/Instance.scala `
    src/main/scala/sjvm/runtime/VirtualMachine.scala `
    src/main/scala/sjvm/CmdlineArguments.scala `
    src/main/scala/sjvm/Main.scala `
    -d out/