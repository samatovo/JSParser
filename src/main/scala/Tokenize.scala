import jdk.jshell.spi.ExecutionControl.NotImplementedException

object Tokenize {
  def isStartToken(ch: Char) = ch == '_' || ch.isLetter
  def isInToken(ch: Char) = ch == '_' || ch.isLetter || ch.isDigit
  def isSymbol(ch: Char) = "!%^&*()-_+=[{]};:|~<,>.?" contains ch
  def isStringStart(ch: Char) = "\"'`" contains ch
  def isStartNumber(ch: Char) = ch.isDigit
  def isWhiteSpace(ch: Char) = " \r\n\t" contains ch


  def apply(str: String): List[String] = apply(str.toList)
  def apply(str: List[Char]): List[String] = {
      if (str.isEmpty) Nil
      else if (isWhiteSpace(str.head)) {
        val (_, rest) = readSequence(str, isWhiteSpace)
        apply(rest)
      }
      else if (isStartToken(str.head)) {
        val (res, rest) = readSequence(str, isInToken)
        res ++ apply(rest)
      }
      else if (isStringStart(str.head)) {
        val (res, rest) = readString(str)
        List(res) ++ apply(rest)
      }
      else if (isStartNumber(str.head)) {
        val (res, rest) = readNumber(str)
        res ++ apply(rest)
      }
      else if (isSymbol(str.head)) {
        List(str.head.toString) ++ apply(str.tail)
      }
      else throw UnexpectedCharacterException(str.head)
  }

  private def readNumber(str: List[Char]): (List[String], List[Char]) = {
    readSequence(str, isStartNumber)
  }


  private def readString(str: List[Char]): (String, List[Char]) = {
    val term = str.head

    def readUntil(acc: String, term: Char, str: List[Char]): (String, List[Char]) = str match {
      case Nil => throw EndOfStreamWhileLookingForException(term)
      case `term` :: rest => (s"$acc$term", rest)
      case '\\' :: Nil =>  throw EndOfStreamWhileLookingForException(term)
      case '\\' :: `term` :: rest => readUntil(s"$acc\\$term", term, rest)
      case '\\' :: _ :: rest => throw new NotImplementedException("string escape sequences")
      case ch :: tail => readUntil(s"$acc$ch", term, tail)
    }

    readUntil(term.toString, term, str.tail)
  }

  private def readSequence(str: List[Char], fn: Char => Boolean): (List[String], List[Char]) = {
    str.indexWhere(!fn(_)) match {
      case -1 => (List(str.mkString), Nil)
      case n => {
        val (letters, rest) = str.splitAt(n)
        (List(letters.mkString), rest)
      }
    }
  }
}

abstract class CharacterException(reason: String, ch: Char)
  extends Exception(s"$reason: »$ch« (U+${"%04x".format(ch.toInt)} ${Character.getName(ch.toInt)})")

final case class UnexpectedCharacterException(ch: Char)
  extends CharacterException("Unexpected character", ch)

final case class EndOfStreamWhileLookingForException(ch: Char)
  extends CharacterException("End of stream while looking for character", ch)
