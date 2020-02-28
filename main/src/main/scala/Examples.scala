package core

/**
 * This is the basic structure to use and define macros.
 * First of all, you need to define macros in another compilation round.
 * In general, is preferred to keep all macros separated from the standard source code.
 * To use def macros, you need to indicate the expansion to call preceded by macro keyword.
 */
object Example0 extends App {
  def foo() : Unit = macro Macros.none
  foo()
}

/**
 * Example of usage of standard log
 */
object Example1 extends App {
  def log(msg : String) : Unit = macro Macros.logImpl
  log("ciao")
}

/**
 * More complex example, it adds class info at compile time
 */
object Example2 extends App {
  def log(msg : String) : Unit = macro Macros.logWithClass
  log("ciao")

  object Inner {
    def hello() : Unit = log("hello")
  }
  Inner.hello()
}

/**
 * Example of runtime reflection. To use this technique you need to create
 * a universe (with runtimeMirror).
 * To analyze the structure of some object/class, you need to create a mirror, with reflect method.
 */
object Example3 extends App {
  import scala.reflect.runtime.universe._
  val ru = runtimeMirror(this.getClass.getClassLoader)
  def annotations(any : Any) : Seq[Annotation] = {
    val mirror = ru.reflect(any)
    mirror.symbol.annotations
  }
  println(annotations(10))

  @Deprecated
  case class Foo(foo : Int)
  println(annotations(Foo(10)))
}

/**
 * Example of usage of a tokenizer macro, all expansions are made at compile-time.
 * It is very important, this means that there isn't overload at runtime
 * to compute this method.
 */
object Example4 extends App {
  case class Student(name : String, surname : String, age : Int)
  class Animal(val name : String)
  class Dog(override val name : String, val owner : String) extends Animal(name)
  def tokenizer(obj : Any) : String = macro Macros.tokenizer
  val student = Student("gianluca", "aguzzi", 24)
  val cat = new Animal("Mimmo")
  val dog = new Dog("Dada", "Marta")
  println(tokenizer(student))
  println(tokenizer(cat))
  println(tokenizer(dog))
}

/**
 * A runtime version of a tokenizer, using reflection.
 */
object Example5 extends App {
  def reflectionTokenizer(element : Any): String = {
    import scala.reflect.runtime.universe._
    val ru = runtimeMirror(this.getClass.getClassLoader)
    val mirror = ru.reflect(element)
    val declarations = mirror.symbol.toType.members
    val getters = declarations.collect {
      case m : MethodSymbol if(m.isGetter && m.isPublic) => m
    }
    getters.map(getter => s"${getter.name}:${mirror.reflectMethod(getter).apply()}")
        .mkString("\n")
  }
  case class Student(name : String, favouriteSubject : String)
  println(reflectionTokenizer(Student("gianluca", "metaprogramming")))
}

/**
 * Example of macro annotations.
 * It is a very powerful mechanism that opens a new perspective.
 * With this technique it is possible to add methods at compile time, do structural checking on the annotated concept and so on.
 * It this case the foo annotation adds a method called foo, which doesn't do anything special.
 */
object Example6 extends App {
  import Annotations._
  @foo
  case class Man(name : String)

  val man = Man("gianluca")
  man.foo()
}

/**
 * Example of tokenizeit annotation. IDEA doesn't fully support macro system.
 * In this case, the tokens value is computed at compile time.
 */
object Example7 extends App {
  import Annotations._
  @tokenizeit
  class Man(val name : String, val surname : String, val age : Int)

  val man = new Man("gianluca", "aguzzi", 24)
  println(man.tokens)

  val name : String = man.tokens._1
  val age : Int = man.tokens._3
}