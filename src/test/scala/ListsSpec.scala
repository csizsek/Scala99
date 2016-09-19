import org.scalatest._

class ListsSpec extends FlatSpec {

  behavior of "last"

  it should "return the last element of a list longer than one" in {
    assert(Lists.last(List(1, "foo", 3)).get == 3)
  }

  it should "return the only element of a list of length one" in {
    assert(Lists.last(List('x')).get == 'x')
  }

  it should "return None for an empty list" in {
    assert(Lists.last(List()).isEmpty)
  }

  behavior of "penultimate"

  "penultimate" should "return the last but one element of a list longer than two" in {
    assert(Lists.penultimate(List(1, 2, 3, 4)).get == 3)
  }

  it should "return the first element of a list of length two" in {
    assert(Lists.penultimate(List("foo", "bar")).get == "foo")
  }

  it should "return None for a list of length one" in {
    assert(Lists.penultimate(List(1)).isEmpty)
  }

  it should "return None for an empty list" in {
    assert(Lists.penultimate(List()).isEmpty)
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
    assert(Lists.nth(3, List("foo", "bar", "baz")).isEmpty)
  }

  it should "return None if n negative" in {
    assert(Lists.nth(-1, List(1, 2, 3)).isEmpty)
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

  it should "pack identical elements together" in {
    assert(Lists.pack(List(1, 1, 2, 3, 3, 3, 4)) == List(List(1, 1), List(2), List(3, 3, 3), List(4)))
    assert(Lists.pack(List(1, 1, 1, 1)) == List(List(1, 1, 1, 1)))
    assert(Lists.pack(List(1, 1, 1, 1, 2, 1, 1, 1, 1)) == List(List(1, 1, 1, 1), List(2), List(1, 1, 1, 1)))
  }

  it should "not pack different elements together" in {
    assert(Lists.pack(List(1, 2, 3, 4)) == List(List(1), List(2), List(3), List(4)))
    assert(Lists.pack(List(1)) == List(List(1)))
  }

  behavior of "encode"

  it should "encode identical element runs" in {
    assert(Lists.encode(List(1, 1, 2, 3, 3, 3, 4)) == List((2, 1), (1, 2), (3, 3), (1, 4)))
  }

  behavior of "encodeModified"

  it should "encode identical element runs of length two or more" in {
    assert(Lists.encodeModified(List(1, 1, 2, 3, 3, 3, 4)) == List((2, 1), 2, (3, 3), 4))
  }

  behavior of "decode"

  it should "decode encoded element runs" in {
    assert(Lists.decode(List((2, 1), (1, 2), (3, 3), (1, 4))) == List(1, 1, 2, 3, 3, 3, 4))
  }

  behavior of "duplicate"

  it should "duplicate all elements" in {
    assert(Lists.duplicate(List(1, 2, 3, 1)) == List(1, 1, 2, 2, 3, 3, 1, 1))
    assert(Lists.duplicate(List()) == List())
    assert(Lists.duplicate(List("foo")) == List("foo", "foo"))
  }

  behavior of "duplicateN"

  it should "duplicate all elements N times" in {
    assert(Lists.duplicateN(2, List(1, 2, 3, 1)) == List(1, 1, 2, 2, 3, 3, 1, 1))
    assert(Lists.duplicateN(1, List(1, 2)) == List(1, 2))
    assert(Lists.duplicateN(3, List("foo", "bar", "baz")) == List("foo", "foo", "foo", "bar", "bar", "bar", "baz", "baz", "baz"))
    assert(Lists.duplicateN(0, List(1, 2, 3)) == List())
    assert(Lists.duplicateN(5, List()) == List())
  }

  behavior of "drop"

  it should "drop every Nth element" in {
    assert(Lists.drop(2, List(1, 2, 1, 2)) == List(1, 1))
    assert(Lists.drop(3, List(1, 2, 3, 1, 2, 3)) == List(1, 2, 1, 2))
    assert(Lists.drop(1, List(1, 2, 1, 2)) == List())
    assert(Lists.drop(10, List(1, 2, 1, 2)) == List(1, 2, 1, 2))
  }

  behavior of "split"

  it should "split the list at the specified place" in {
    assert(Lists.split(2, List(1, 2, 3, 4)) == (List(1, 2), List(3, 4)))
    assert(Lists.split(1, List(1, 2, 3, 4, 5, 6)) == (List(1), List(2, 3, 4, 5, 6)))
    assert(Lists.split(0, List(1, 2, 3, 4)) == (List(), List(1, 2, 3, 4)))
    assert(Lists.split(2, List()) == (List(), List()))
  }


}
