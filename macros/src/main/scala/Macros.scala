package core
import java.util.Date

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox.{Context => BlackBox}
import scala.reflect.macros.whitebox.{Context => Whitebox}
object Macros {
  /**
   * Not very useful, this macro doesn't do anything.
   */
  def none(c : BlackBox)() = {
    c.universe.reify()
  }
  /**
   * It logs the message adding the current date.
   * Nothing special, this can be done easily with standard programming techniques.
   */
  def logImpl(c : BlackBox)(msg : c.Expr[String]) : c.Expr[Unit] = {
    import c.universe._
    val date = reify(new Date().toString) //BN! it is different from new Date().toString! With reify the code isn't evaluate a compile time! otherwhise the date corresponding with the compiling time
    c.Expr(q"""println($date + $msg)""") //q"..." create a Tree, to return a Expr it only need to wrap the tree into the costructor
  }

  /**
   * It is a bit more complex function, it logs message and some information about the enclosing class.
   */
  def logWithClass(c : Whitebox)(msg : c.Expr[String]) : c.Expr[Unit] = {
    import c.universe._
    def isEncloser(symbol: Symbol) : Boolean = symbol.isClass || symbol.isModule
    def parentDefSymbol(symbolTree : Symbol) : Symbol = if(isEncloser(symbolTree))
      symbolTree
    else
      parentDefSymbol(symbolTree.owner)
    val parentSymbol = parentDefSymbol(c.internal.enclosingOwner)
    val name = parentSymbol.name.toString
    c.Expr(q"""println(${name} + ":" + $msg)""")
  }

  /**
   * This function creates, for each value in the object, a name-value pair.
   * This example could be expanded to create JSON from some object.
   */
  def tokenizer(c : Whitebox)(obj : c.Expr[Any]) : c.Expr[String] = {
    import c.universe._
    val ast = obj.tree
    //from an object, retrieves all his getters
    val getters = ast match {
      case Select(_, _) =>
        val tpeDeclaration = ast.tpe.members
        tpeDeclaration
          .collect({
            case m : MethodSymbol if(m.isGetter && m.isPublic) => m.asMethod
          })
          .map(_.name)
      case _ =>
        c.abort(ast.pos, "tokenizer not allowed here")
    }
    val tokenizerCode = getters.map(valName => q"""${valName.toString} + ":" + $obj.$valName""")
    val code = q"""List(..$tokenizerCode).mkString("{\n", "\n", "\n}") """
    c.Expr(code)
  }
}

object Annotations {

  /**
   * nothing special, foo annotations add foo method that prints 'hello'
   */
  @compileTimeOnly("enable macro paradise to expand macro annotations")
  class foo extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro fooImpl
  }
  def fooImpl(c: Whitebox)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val inputs = annottees.map(_.tree).toList
    val annotated = inputs.collectFirst {
      case classDef: ClassDef => classDef
    }
    annotated match {
      case Some(classDef @ ClassDef(mods, tpe, params, template)) => {
        val foo = q"""val foo = () => println("hello")"""
        val bodyWithFoo = foo :: classDef.impl.body
        val templateWithFoo = Template(template.parents, template.self, bodyWithFoo)
        val classWithFoo = ClassDef(mods, tpe, params, templateWithFoo)
        c.Expr[Any](classWithFoo)
      }
      case None => c.abort(c.enclosingPosition, "annotations not allowed here")
    }
  }

  /**
   * Similar to tokenizer macro, these annotations allow to create a set of tokens
   * based on the values of an object.
   * In this case, the annotations add a value called tokens that contains
   * a tuple of the value defined in the class.
   */
  @compileTimeOnly("enable macro paradise to expand macro annotations")
  class tokenizeit extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro tokenizeitImpl
  }
  def tokenizeitImpl(c: Whitebox)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val inputs = annottees.map(_.tree).toList
    val annotated = inputs.collectFirst {
      case classDef: ClassDef => classDef
    }
    annotated match {
      case Some(classDef @ ClassDef(mods, tpe, params, template)) => {
        val vals = template.body.collect {
          case ValDef(mod, name, tpt @ Ident(tpe), _) =>
            (name, tpe)
        }
        val values = vals.map(value => q"this.${value._1}")
        val valuesCode = q"(..$values)"

        val tokens = q"""lazy val tokens = $valuesCode """
        val bodyWithTokens = tokens :: classDef.impl.body
        val templateWithTokens = Template(template.parents, template.self, bodyWithTokens)

        val classWithTokens = ClassDef(mods, tpe, params, templateWithTokens)
        c.Expr[Any](classWithTokens)
      }
      case None => c.abort(c.enclosingPosition, "annotations not allowed here")
    }
  }
}
