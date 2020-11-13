import org.scalatest.flatspec.AnyFlatSpec

class TokenizeTests extends AnyFlatSpec {
  "An empty string" should "return Nil" in {
    assert(Tokenize("") == Nil)
  }

  "A single or multiple letters" should "return those letter(s)" in {
    assert(Tokenize("a") == List("a"))
    assert(Tokenize("z") == List("z"))
    assert(Tokenize("abc") == List("abc"))
    assert(Tokenize("cat") == List("cat"))
  }

  "A simple number" should "return that number" in {
    assert(Tokenize("0") == List("0"))
    assert(Tokenize("1") == List("1"))
    assert(Tokenize("100") == List("100"))
    assert(Tokenize("1234") == List("1234"))
  }

  "A single symbol" should "return that symbol" in {
    assert(Tokenize("+") == List("+"))
    assert(Tokenize("(") == List("("))
  }

  "Multiple symbols" should "return them separately" in {
    assert(Tokenize("+-") == List("+", "-"))
    assert(Tokenize("()") == List("(", ")"))
  }

  "Letter(s) then symbol" should "return both" in {
    assert(Tokenize("a+") == List("a", "+"))
    assert(Tokenize("alpha-") == List("alpha", "-"))
  }

  "Letter(s) separated by symbol" should "return all" in {
    assert(Tokenize("a+b") == List("a", "+", "b"))
    assert(Tokenize("alpha-beta") == List("alpha", "-", "beta"))
  }

  "Letter(s) separated by white space" should "return both" in {
    assert(Tokenize("a b") == List("a", "b"))
    assert(Tokenize("alpha beta") == List("alpha", "beta"))
    assert(Tokenize("alpha\nbeta") == List("alpha", "beta"))
    assert(Tokenize("alpha\t \nbeta") == List("alpha", "beta"))
  }

  "Simple strings" should "read the whole string" in {
    assert(Tokenize("'cheese'") == List("'cheese'"))
    assert(Tokenize("\"cheese\"") == List("\"cheese\""))
  }

  "Strings with escaped quotes" should "read the whole string" in {
    assert(Tokenize("'ch\\'eese'") == List("'ch\\'eese'"))
    assert(Tokenize("\"ch\\\"eese\"") == List("\"ch\\\"eese\""))

  }
}
