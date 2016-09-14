import org.scalatest._

class ListsSpec extends FlatSpec {

  behavior of "last"

  it should "return the last element of a list longer than one" in {
    assert(Lists.last(List(1, 2, 3)).get == 3)
  }

  it should "return the only element of a list of length one" in {
    assert(Lists.last(List('x')).get == 'x')
  }

  it should "return None for an empty list" in {
    assert(Lists.last(List()) == None)
  }

  behavior of "penultimate"

  "penultimate" should "return the last but one element of a list longer than two" in {
    assert(Lists.penultimate(List(1, 2, 3, 4)).get == 3)
  }

  it should "return the first element of a list of length two" in {
    assert(Lists.penultimate(List("foo", "bar")).get == "foo")
  }

  it should "return None for a list of length one" in {
    assert(Lists.penultimate(List(1)) == None)
  }

  it should "return None for an empty list" in {
    assert(Lists.penultimate(List()) == None)
  }

  behavior of "nth"

  it should "return the correct element in a list longer than one" in {
    assert(Lists.nth(1, List("a", "b", "c")).get == "b")
    assert(Lists.nth(0, List(1, 2, 3)).get == 1)
    assert(Lists.nth(4, List(1, 2, 3, 4, 5)).get == 5)
  }

  it should "return the correct element in a one element list" in {
    assert(Lists.nth(0, List(1)).get == 1)
  }

  it should "return None if n is out of bounds" in {
    assert(Lists.nth(3, List("foo", "bar", "baz")) == None)
  }

  it should "return None if n negative" in {
    assert(Lists.nth(-1, List(1, 2, 3)) == None)
  }

  behavior of "length"

  it should "return the length of the list" in {
    assert(Lists.length(List(1, 2, "foo", 6.0)) == 4)
    assert(Lists.length(List("hello")) == 1)
    assert(Lists.length(List()) == 0)
  }

  behavior of "reverse"

  it should "reverse a list longer than two" in {
    assert(Lists.reverse(List(1, 2, 3)) == List(3, 2, 1))
  }

  it should "not change a list shorter than two" in {
    assert(Lists.reverse(List("foo")) == List("foo"))
    assert(Lists.reverse(List()) == List())
  }

  behavior of "isPalindrome"

  it should "recognize palindromes" in {
    assert(Lists.isPalindrome(List(2, 2)))
    assert(Lists.isPalindrome(List(2, 3, 3, 2)))
    assert(Lists.isPalindrome(List(2, 3, 4, 3, 2)))
    assert(Lists.isPalindrome(List(2)))
    assert(Lists.isPalindrome(List()))
  }

  it should "not recognize non-palindromes" in {
    assert(!Lists.isPalindrome(List(2, 3)))
    assert(!Lists.isPalindrome(List(2, 3, 4, 2)))
    assert(!Lists.isPalindrome(List(2, 3, 4, 2, 3)))
  }

  behavior of "flatten"

  it should "flatten deep lists" in {
    assert(Lists.flatten(List(List(1, 2), 3)) == List(1, 2, 3))
    assert(Lists.flatten(List(List(1, 2), 3, List(4, List(5, 6)))) == List(1, 2, 3, 4, 5, 6))
    assert(Lists.flatten(List(List(List(List(1, 2, 3))))) == List(1, 2, 3))
  }

  it should "not change shallow lists" in {
    assert(Lists.flatten(List(1, 2, 3)) == List(1, 2, 3))
    assert(Lists.flatten(List(1)) == List(1))
    assert(Lists.flatten(List()) == List())
  }

  behavior of "compress"

  it should "compress compressable sequences" in {
    assert(Lists.compress(List(1, 2, 2, 3, 4, 4, 4, 5, 6, 6)) == List(1, 2, 3, 4, 5, 6))
    assert(Lists.compress(List(1, 1, 1, 1, 1)) == List(1))
    assert(Lists.compress(List(2, 1, 1, 1, 1)) == List(2, 1))
    assert(Lists.compress(List(1, 1, 2, 1, 1)) == List(1, 2, 1))
  }

  it should "not change non-compressable sequences" in {
    assert(Lists.compress(List(1, 2, 3, 4, 5, 6)) == List(1, 2, 3, 4, 5, 6))
    assert(Lists.compress(List(1)) == List(1))
    assert(Lists.compress(List(2, 1)) == List(2, 1))
    assert(Lists.compress(List()) == List())
  }

  behavior of "pack"

  it should "pack elements together" in {
    //assert(Lists.pack(List(1, 1, 2, 3, 3, 3, 4)) == List(List(1, 1), List(2), List(3, 3, 3), List(4)))
  }

}
