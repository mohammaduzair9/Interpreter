package ASTBuilderParser
import LexicalAnalyzer._

object AST {

  trait node

  case class Nil        (Token:Token) extends node
  case class Integer    (Token:Token) extends node
  case class Bool       (Token:Token) extends node
  case class Alpha      (Token:Token) extends node
  case class Var        (Token: Token) extends node
  case class Const      (Token: Token) extends node
  case class Print      (Value:node) extends node
  case class UnaryOp    (Token: Token, Op: node) extends node
  case class BinaryOp   (LeftOp: node, Token: Token, RightOp: node) extends node
  case class Declare    (Name : node, Token: Token, Value: node) extends node
  case class Assign     (Name:node, Token: Token, Value: node) extends node
  case class IfElse     (If: node, Then: node, Else: node) extends node
  case class While      (While:node, Do:List[node]) extends node
  case class Block      (Statements: List[node]) extends node
  case class SyntaxTree (nodes: List[node]) extends node

}
