package scala.lms

import scala.collection.mutable.{Map, Stack}

class ScopedVarMap[T]() {
  private val outermost: Map[String, T] = Map()
  private val scopes: Stack[Map[String, T]] = Stack()

  def open(): Unit = {
    scopes.push(Map())
  }

  def close(): Unit = {
    scopes.pop()
  }

  def add(k: String, v: T) = scopes.headOption match {
    case Some(s) => s.addOne((k, v))
    case None => outermost.addOne((k, v))
  }

  def lookup(k: String): Option[T] = {
    (scopes.iterator ++ Iterator(outermost)).collectFirst(Function.unlift(_.get(k)))
  }
}
