import Arithmetic.S99Int
import org.scalatest._

class ArithmeticSpec extends FlatSpec {

  behavior of "isPrime"

  it should "return true for primes" in {
    assert((new S99Int(2)).isPrime())
    assert((new S99Int(3)).isPrime())
    assert((new S99Int(7)).isPrime())
    assert((new S99Int(1023)).isPrime())
  }

  /*
  it should "return false for non-primes" in {
    assert(!(new S99Int(1)).isPrime())
    assert(!(new S99Int(0)).isPrime())
    assert(!(new S99Int(6)).isPrime())
    assert(!(new S99Int(-5)).isPrime())
  }
  */
}
