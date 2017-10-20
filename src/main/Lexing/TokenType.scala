package Lexing

object TokenType {

  sealed trait _type

  case object IDENTIFIER extends _type
  case object INTEGER extends _type
  case object BOOLEAN extends _type
  case object ALPHA extends _type
  case object CONST_TYPE extends _type
  case object SKIP extends _type
  case object STRING extends _type
  case object ASSIGNMENT extends _type
  case object VAR_TYPE extends _type
  case object COLON extends _type
  case object EMPTY extends _type
  case object SPACE extends _type
  case object BOP extends _type
  case object PLUS extends _type
  case object MUL extends _type
  case object DIV extends _type
  case object UOP extends _type
  case object DATA_TYPE extends _type
  case object RESERVED extends _type
  case object BREAK extends _type
  case object WHILE extends _type
  case object DO extends _type
  case object IF extends _type
  case object THEN extends _type
  case object ELSE extends _type
  case object NIL extends _type
  case object PRINT extends _type

}
