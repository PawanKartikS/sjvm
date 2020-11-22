SJVM is an implementation of JVM (Java Virtual Machine) in Scala.

### Progress -
1. Implements close to 60% of the actual instruction set.
2. Capable of class instantiation, invoking constructors & methods (static and non-static), throwing and catching exceptions,
object casting, etc.

### Incoming support for -
1. Correctly handle `jmodules`: this will allow you to call for example - `System.out.println()`, `new ArrayList<T>()`, etc.

A very tiny example -
```java
class Person {
  private String name;
  private int age;
  private int someval;

  // Main constructor
  public Person(String name, int age) {
    this.name = name;
    this.age = age;
  }

  // overloaded
  public Person(String name, int age, Object o) {
    this(name, age);

    // capable of checking for and casting
    if (o instanceof String) {
      this.age--;
    }
  }

  public void setSomeval(int someval) {
    int c = 0;
    for (int i = 1; i <= someval; i++) {
      c += (i % 2 == 0) ? i + 1 : i - 1;
    }

    this.someval = c;
  }
}

public class Main {
  public static void main(String[] args) {
    // capable of resolving to the correctly overloaded constructor/method
    var me = new Person("pawan", 22);
    me.setSomeval(30);
  }
}
```

Executing the above code using sjvm -
```
> scala -classpath . sjvm.Main Main.class Person.class -jvm:ShowClassFields -jvm:MainClass=Main
cannot possibly fully init jvm, missing rt path

name -> "pawan"
someval -> 465
age -> 22
```
