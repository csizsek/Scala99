object Arithmetic {

  class S99Int(val start: Int) {

    def isPrime(): Boolean = {
      true
    }

  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
  }



}
