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
    check("virtualize-if-basic", Snippet.code)
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
    check("virtualize-if-nested", Snippet.code)
  }

  test("while empty") {
    object Snippet extends DslDriver[Boolean, Int] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Int] = {
        while(x) {
        }
        0
      }
    }
    check("virtualize-while-empty", Snippet.code)
  }

  /*
  test("while2") {
    object Snippet extends DslDriver[Int, Int] with Dsl {
      def snippet(x: Rep[Int]): Rep[Int] = {
        var y = x
        while (y < 10) {
          y = y + 1
        }
        y
      }
    }
  }
  */
}
