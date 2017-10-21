package LexicalAnalyzer

import scala.util.matching.Regex

object TokenRegex {

  val DATA_TYPE = "^(int|bool|alpha)$".r
  val INTEGER = "-?\\d+".r
  val BOOLEAN = "^(tt|ff)$".r
  val ALPHA = "^([\"|\']).*([\"|\'])$".r
  val IDENTIFIER = "[a-zA-Z][A-Za-z0-9_$*#]*".r
  val VAR_TYPE = "(var)".r
  val CONST_TYPE = "(const)".r
  val IF = "(if)".r
  val THEN = "(then)".r
  val ELSE = "(else)".r
  val DO = "(do)".r
  val WHILE = "(while)".r
  val COLON = "[:]{1}".r
  val ASSIGNMENT = "[=]{1}".r
  val SPACE = " ".r
  val BREAK = """\n|;""".r
  val NIL = "(nil)".r
  val UOP = "-|not".r
  val BOP = "==|><|and|or|\\^|>|<".r
  val PLUS = "\\+".r
  val MUL = "\\*".r
  val DIV = "\\/".r
  val PRINT = "(print)".r
  val SKIP = "^(skip)$".r

}
