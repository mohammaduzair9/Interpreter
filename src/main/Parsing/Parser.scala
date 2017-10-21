package Parsing

import Lexing._
import Parsing.AST._
import scala.util.{Failure, Success, Try}

class Parser(lexer: Lexer) {

  def getNextToken(tokens: List[Token]) : List[Token] = if(!tokens.isEmpty ) tokens.tail else tokens

  def program(tokens : List[Token]): (List[Token], node) = block(tokens)

  def block(tokens : List[Token]): (List[Token], node) = {
    val (newTokens, nodes) = statements(tokens)
    (newTokens, AST.Block(nodes))
  }

  def statements(tokens : List[Token]): (List[Token], List[node]) = {
     getStatements(tokens,List())
  }

  def getStatements(tokens : List[Token], nodes : List[node]): (List[Token], List[node]) = {
    if(tokens.isEmpty) (tokens,nodes)
    else{
      val (nextTokens, node) = statement(tokens)
      val newNodes: List[node] = nodes :+ node

      if(nextTokens.isEmpty) (nextTokens,newNodes)
      else if(nextTokens.head.tokentype != TokenType.BREAK) throw new Exception("Semicolon not found")
      else getStatements(getNextToken(nextTokens), newNodes)
    }
  }

  def statement(tokens : List[Token]): (List[Token], node) = {

    tokens.head.tokentype match {
      case TokenType.VAR_TYPE |
           TokenType.CONST_TYPE   => declaration(tokens)
      case TokenType.SKIP         => skip_statement(tokens)
      case TokenType.IF           => if_statement(tokens)
      case TokenType.IDENTIFIER   => assign_statement(tokens)
      case TokenType.WHILE        => while_statement(tokens)
      case TokenType.PRINT        => print_statement(tokens)
      case _                      => throw new Exception("No more statements")
    }
  }

  def declaration(tokens : List[Token]): (List[Token], node) = {
    val (nextTokens, left_node) = variable(getNextToken(tokens))
    if(!(nextTokens.head.tokentype == TokenType.COLON)) throw new Exception("Syntax Error : COLON Not found")
    val nextTokens2 = getNextToken(nextTokens)
    val data_type_token = nextTokens2.head
    if(!(nextTokens2.head.tokentype == TokenType.DATA_TYPE)) throw new Exception("Syntax Error : DATATYPE Not found")
    val nextTokens3 = getNextToken(nextTokens2)
    if(nextTokens3.head.tokentype != TokenType.ASSIGNMENT){
      if (nextTokens3.head.tokentype == TokenType.BREAK){
        val next_node = Declare(left_node, nextTokens.head, null)
        return (nextTokens3, next_node)
      }
      else throw new Exception("Error: Invalid Assignment Statement")
    }
    val nextTokens4 = getNextToken(nextTokens3)
    if (data_type_token.value.equals("int")){
      Try(nextTokens4.head.value.toInt).getOrElse(
        if(nextTokens4.head.tokentype == TokenType.NIL) Success
        else if(nextTokens4.head.tokentype == TokenType.IDENTIFIER) Success
        else throw new Exception("Error: Value should be Integer")
      )
    } else if (data_type_token.value.equals("bool")){
      Try(nextTokens4.head.value.toBoolean).getOrElse(
        if(nextTokens4.head.tokentype == TokenType.NIL) Success
        else if(nextTokens4.head.tokentype == TokenType.IDENTIFIER) Success
        else throw new Exception("Error: Value should be Boolean")
      )
    } else if (data_type_token.value.equals("alpha")){
      if (nextTokens4.head.value.charAt(0).!=('\"')){
        if(nextTokens4.head.tokentype == TokenType.NIL) Success
        else if(nextTokens4.head.tokentype == TokenType.IDENTIFIER) Success
        else throw new Exception("Error: Value should be Alpha")
      }
    }

    val (nextTokens5, right_node) = expr(nextTokens4)
    val next_node = Declare(left_node, nextTokens.head, right_node)
    return (nextTokens5, next_node)
  }

  def skip_statement(tokens: List[Token]): (List[Token],node) = {
    if (tokens.head.tokentype == TokenType.BREAK) statement(tokens)
    else skip_statement(getNextToken(tokens))
  }

  def if_statement(tokens : List[Token]): (List[Token], node) = {
    val (thenToken, expr_node) = expr(getNextToken(tokens))
    if (thenToken.head.tokentype != TokenType.THEN) throw new Exception("Syntax Error : THEN Not found")
    else{
      val (elseToken, statement_node) = statement(getNextToken(thenToken))
      if (elseToken.head.tokentype != TokenType.ELSE) throw new Exception("Syntax Error : ELSE Not found")
      else{
        val (endIfTokens, second_statement_node) = statement(getNextToken(elseToken))
        val if_node = IfElse(expr_node,statement_node,second_statement_node)
        (endIfTokens, if_node)
      }
    }
  }

  def assign_statement(tokens : List[Token]): (List[Token], node) = {
    val left_node = Var(tokens.head)
    val AssignToken = getNextToken(tokens)
    if(AssignToken.head.tokentype != TokenType.ASSIGNMENT) throw new Exception("Syntax Error : = Not found")
    else {
      val (nextTokens, right_node) = expr(getNextToken(AssignToken))
      val next_node = Assign(left_node, AssignToken.head, right_node)
      (nextTokens, next_node)
    }
  }

  def while_statement(tokens : List[Token]): (List[Token], node) = {
    val (doToken, expr_node) = expr(getNextToken(tokens))
    if(doToken.head.tokentype != TokenType.DO) throw new Exception("Syntax Error : DO Not found")
    val (nextTokens, statement_node) = statement(getNextToken(doToken))
    (nextTokens, While(expr_node, statement_node))
  }

  def print_statement(tokens : List[Token]): (List[Token], node) = {
    val (nextTokens, expr_node) = expr(getNextToken(tokens))
    (nextTokens, Print(expr_node))
  }

  def factor(tokens : List[Token]): (List[Token], node) = {

    tokens.head.tokentype match {
      case TokenType.UOP      => val(nextTokens, node) = factor(getNextToken(tokens));return (nextTokens, UnaryOp(tokens.head, node));
      case TokenType.INTEGER  => (getNextToken(tokens), Integer(tokens.head))
      case TokenType.BOOLEAN  => (getNextToken(tokens), Boolean(tokens.head))
      case TokenType.ALPHA    => (getNextToken(tokens), Alpha(tokens.head))
      case TokenType.NIL      => (getNextToken(tokens), Nil(tokens.head))
      case _                  => (getNextToken(tokens), Var(tokens.head))
    }

  }

  def term(tokens : List[Token]): (List[Token], node) = {val (nextTokens, left_node) = factor(tokens);  nextTerm(nextTokens, left_node)}
  def nextTerm(tokens: List[Token], left_node: node): (List[Token], node) = {
    if (tokens.isEmpty) (tokens, left_node)
    else {
      tokens.head.tokentype match {
        case TokenType.DIV | TokenType.MUL => {
          val (nextTokens, rightNode) = factor(getNextToken(tokens));
          val node = BinaryOp(left_node, tokens.head, rightNode)
          nextTerm(nextTokens, node)
        }
        case _ => (tokens, left_node)
      }
    }
  }

  def expr(tokens:List[Token]): (List[Token], node) = {val (newTokens, left_node) = term(tokens); nextExpr(newTokens, left_node)}
  def nextExpr(tokens : List[Token], left_node: node): (List[Token],node) = {
    if (tokens.isEmpty) (tokens, left_node)
    else {
      tokens.head.tokentype match {
        case TokenType.BOP | TokenType.PLUS => {
          val (nextTokens, rightNode) = term(getNextToken(tokens))
          val node = BinaryOp(left_node, tokens.head, rightNode)
          nextExpr(nextTokens, node)
        }
        case _ => (tokens, left_node)
      }
    }
  }

  def variable(tokens : List[Token]): (List[Token], node) = (getNextToken(tokens), Var(tokens.head))

  def const(tokens : List[Token]): (List[Token], node) = (getNextToken(tokens), Const(tokens.head))

  def parse(): node = {
    val (theTokens,tree) = program(lexer.lexData)
    println(tree)
    return tree
  }
}
