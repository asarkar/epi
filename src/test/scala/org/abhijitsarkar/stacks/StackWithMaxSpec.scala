package org.abhijitsarkar.stacks

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class StackWithMaxSpec extends FlatSpec {
  "Stack" should "support max operation" in {
    val stack = new StackWithMax[Int]()
    stack.push(2)
    stack.max shouldBe 2
    stack.push(2)
    stack.max shouldBe 2
    stack.push(1)
    stack.max shouldBe 2
    stack.push(4)
    stack.max shouldBe 4
    stack.push(5)
    stack.max shouldBe 5
    stack.push(5)
    stack.max shouldBe 5
    stack.push(3)
    stack.max shouldBe 5
    stack.pop() shouldBe 3
    stack.max shouldBe 5
    stack.pop() shouldBe 5
    stack.max shouldBe 5
    stack.pop() shouldBe 5
    stack.max shouldBe 4
    stack.pop() shouldBe 4
    stack.max shouldBe 2
    stack.push(0)
    stack.max shouldBe 2
    stack.push(3)
    stack.max shouldBe 3
  }

}
