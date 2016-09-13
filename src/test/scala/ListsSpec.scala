import org.scalatest._

class ListsSpec extends FlatSpec {

  "last" should "return the last element of a list longer than one" in {
    assert(Lists.last(List(1, 2, 3)).get == 3)
  }

  "last" should "return the only element of a list of length one" in {
    assert(Lists.last(List(1)).get == 1)
  }

  "last" should "return None for an empty list" in {
    assert(Lists.last(List()) == None)
  }

  "penultimate" should "return the last but one element of a list longer than two" in {
    assert(Lists.penultimate(List(1, 2, 3, 4)).get == 3)
  }

  "penultimate" should "return the first element of a list of length two" in {
    assert(Lists.penultimate(List(1, 2)).get == 1)
  }

  "penultimate" should "return None for a list of length one" in {
    assert(Lists.penultimate(List(1)) == None)
  }

  "penultimate" should "return None for an empty list" in {
    assert(Lists.penultimate(List()) == None)
  }

}
