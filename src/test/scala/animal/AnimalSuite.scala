package animal

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AnimalSuite extends FunSuite {

  def nullOrEmptyListTests(f: (List[Any]) => Any) {
    assertThrows[IllegalArgumentException] {
      f(null)
    }
    assertThrows[IllegalArgumentException] {
      f(List())
    }
  }


  test("1: compare toString") {

    val animal: Animal = new Animal
    val cat: Cat = new Cat

    assert(animal.toString != cat.toString)

  }
}
