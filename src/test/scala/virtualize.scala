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

  test("equality guard") {
    object Snippet extends DslDriver[Int, Int] with Dsl {
      def snippet(x: Rep[Int]): Rep[Int] = {
        if (x == 1) 2 else x
      }
    }
    check("if-tutorial", Snippet.code)
  }

  test("function calls") {
    object Snippet extends DslDriver[Int, Int] with Dsl {
      def snippet(x: Rep[Int]) = {
        def compute(b: Rep[Boolean]): Rep[Int] = {
          // the if is deferred to the second stage
          if (b) 1 else x
        }
        compute(x==1)
      }
    }
    check("func-tutorial", Snippet.code)
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

  test("power") {
    object Snippet extends DslDriver[Int,Int] {
      def square(x: Rep[Int]): Rep[Int] = x*x

      def power(b: Rep[Int], n: Int): Rep[Int] =
        if (n == 0) 1
        else if (n % 2 == 0) square(power(b, n/2))
        else b * power(b, n-1)

      def snippet(b: Rep[Int]): Rep[Int] =
        power(b, 7)
    }
    check("power", Snippet.code)
  }
}
