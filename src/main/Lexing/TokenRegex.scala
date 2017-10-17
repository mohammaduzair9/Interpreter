import scala.util.matching.Regex

object TokenRegex {

  val DATA_TYPE = "int|bool|alpha".r
  val INTEGER = "-?[0-9]+".r
  val BOOLEAN = "tt|ff".r
  val ALPHA = "^([\"|\']).*([\"|\'])$".r
  val IDENTIFIER = "[a-zA-Z][A-Za-z0-9_$*#]*".r
  val VAR_TYPE = "const|var".r
  val IF = "if".r
  val THEN = "then".r
  val ELSE = "else".r
  val DO = "do".r
  val WHILE = "while".r
  val COLON = "[:]{1}".r
  val ASSIGNMENT = "[=]{1}".r
  val SPACE = " ".r
  val BREAK = "\\n|;".r
  val NIL = "nil".r
  val UOP = "-|not".r
  val BOP = "==|><|and|or|\\+|\\*|\\/|\\^|>|<".r
  val PRINT = "print".r

}
