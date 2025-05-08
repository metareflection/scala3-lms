package scala.lms
package tests

import scala.lms.common.*

@virtualize
class VirtualizeTest extends TutorialFunSuite {
  val under = "virtualize/"

  test("simple if") {
    object Snippet extends DslDriver[Boolean, Int] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Int] = {
          if (x) {
            1
          } else {
            0
          }
      }
    }
    check("if-basic", Snippet.code)
  }

  test("if nested") {
    object Snippet extends DslDriver[Boolean, Int] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Int] = {
          if (x) {
            if (x) { 1 } else { 2 }
          } else {
            0
          }
      }
    }
    check("if-nested", Snippet.code)
  }

  test("if tutorial") {
    object Snippet extends DslDriver[Int, Int] with Dsl {
      def snippet(x: Rep[Int]): Rep[Int] = {
        if (x == 1) 2 else x
      }
    }
    check("if-tutorial", Snippet.code)
  }

  test("while empty") {
    object Snippet extends DslDriver[Boolean, Int] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Int] = {
        while(x) {
        }
        0
      }
    }
    check("while-empty", Snippet.code)
  }

  test("array") {
    object Snippet extends DslDriver[Array[Int], Array[Int]] with Dsl {
      def snippet(x: Rep[Array[Int]]): Rep[Array[Int]] = {
        x(0) = 1
        x
      }
    }
    check("array-assign", Snippet.code)
  }
}
