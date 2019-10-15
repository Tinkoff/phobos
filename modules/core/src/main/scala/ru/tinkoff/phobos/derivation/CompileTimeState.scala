package ru.tinkoff.phobos.derivation

/*
 * Copy-pasted from https://github.com/propensive/magnolia
 */

import scala.collection.mutable
import scala.reflect.macros.blackbox

private[derivation] object CompileTimeState {

  sealed abstract class TypePath(path: String) { override def toString = path }
  final case class CoproductType(typeName: String) extends TypePath(s"coproduct type $typeName")

  final case class ProductType(paramName: String, typeName: String)
    extends TypePath(s"parameter '$paramName' of product type $typeName")

  final case class ChainedImplicit(typeClassName: String, typeName: String)
    extends TypePath(s"chained implicit $typeClassName for type $typeName")

  final class Stack[C <: blackbox.Context with Singleton] {
    private var frames = List.empty[Frame]
    private val cache = mutable.Map.empty[C#Type, C#Tree]

    def isEmpty: Boolean = frames.isEmpty
    def nonEmpty: Boolean = frames.nonEmpty
    def top: Option[Frame] = frames.headOption
    def pop(): Unit = frames = frames drop 1
    def push(frame: Frame): Unit = frames ::= frame

    def clear(): Unit = {
      frames = Nil
      cache.clear()
    }

    def find(searchType: C#Type): Option[C#TermName] = frames.collectFirst {
      case Frame(_, tpe, term) if tpe =:= searchType => term
    }

    def recurse[T <: C#Tree](frame: Frame, searchType: C#Type)(fn: => C#Tree): C#Tree = {
      push(frame)
      val result = cache.getOrElseUpdate(searchType, fn)
      pop()
      result
    }

    def trace: List[TypePath] =
      frames.drop(1).zip(frames).collect {
        case (Frame(path, tp1, _), Frame(_, tp2, _))
          if !(tp1 =:= tp2) => path
      }

    override def toString: String =
      frames.mkString(" a stack:\n", "\n", "\n")

    final case class Frame(path: TypePath, searchType: C#Type, term: C#TermName)
  }

  object Stack {
    // Cheating to satisfy Singleton bound (which improves type inference).
    private val dummyContext: blackbox.Context = null
    private val global = new Stack[dummyContext.type]
    private val workSet = mutable.Set.empty[blackbox.Context#Symbol]

    def withContext(c: blackbox.Context)(fn: Stack[c.type] => c.Tree): c.Tree = {
      workSet += c.macroApplication.symbol
      val depth = c.enclosingMacros.count(m => workSet(m.macroApplication.symbol))
      try fn(global.asInstanceOf[Stack[c.type]])
      finally if (depth <= 1) {
        global.clear()
        workSet.clear()
      }
    }
  }
}