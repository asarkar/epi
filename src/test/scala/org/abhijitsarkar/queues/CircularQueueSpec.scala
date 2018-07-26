package org.abhijitsarkar.queues

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

class CircularQueueSpec extends FlatSpec {
  "Queue" should "do what queues do" in {
    val queue = new CircularQueue[Int](10)

    (1 to 10).foreach(queue.enqueue)
    queue.size shouldBe 10
    queue.isEmpty shouldBe false

    (1 to 9).foreach { x =>
      queue.dequeue().value shouldBe x
      queue.size shouldBe 10 - x
      queue.isEmpty shouldBe false
    }
    queue.dequeue().value shouldBe 10
    queue.isEmpty shouldBe true
    queue.dequeue() shouldBe empty

    (1 to 20).foreach(queue.enqueue)
    queue.size shouldBe 20
    queue.isEmpty shouldBe false
  }
}
