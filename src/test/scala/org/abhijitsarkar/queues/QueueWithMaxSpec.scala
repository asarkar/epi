package org.abhijitsarkar.queues

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class QueueWithMaxSpec extends FlatSpec {
  "Queue" should "support max operation" in {
    val queue = new QueueWithMax[Int]()
    List(3, 1, 3, 2, 0).foreach(queue.enqueue)
    queue.max shouldBe 3

    queue.enqueue(1)
    queue.max shouldBe 3
    queue.dequeue() shouldBe 3
    queue.max shouldBe 3
    queue.dequeue() shouldBe 1
    queue.max shouldBe 3
    queue.enqueue(2)
    queue.max shouldBe 3
    queue.enqueue(4)
    queue.max shouldBe 4
    queue.dequeue() shouldBe 3
    queue.enqueue(4)
    queue.max shouldBe 4
  }
}
