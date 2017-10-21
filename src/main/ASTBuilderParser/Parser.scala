package ASTBuilderParser

import LexicalAnalyzer._
import ASTBuilderParser.AST._
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
      case TokenType.IDENTIFIER   => identifier_statement(tokens)
      case TokenType.ALPHA |
           TokenType.BOOLEAN |
           TokenType.INTEGER      => literal_statement(tokens)
      case TokenType.WHILE        => while_statement(tokens)
      case TokenType.PRINT        => print_statement(tokens)
      case _                      => throw new Exception("No more statements")
    }
  }

  def declaration(tokens : List[Token]): (List[Token], node) = {
    val (nextTokens, left_node) = tokens.head.tokentype match {
      case TokenType.VAR_TYPE     => variable(getNextToken(tokens))
      case TokenType.CONST_TYPE   => const(getNextToken(tokens))
      case _                      => throw new Exception("Syntax Error : VAR OR CONST Not found")
    }
    if(nextTokens.apply(0).tokentype != TokenType.COLON)      throw new Exception("Syntax Error : COLON Not found")
    if(nextTokens.apply(1).tokentype != TokenType.DATA_TYPE)  throw new Exception("Syntax Error : DATATYPE Not found")

    val dataTypeToken = getNextToken(nextTokens)
    val assignToken = getNextToken(dataTypeToken)

    assignToken.head.tokentype match {
      case TokenType.ASSIGNMENT   => {

            val valueToken = getNextToken(assignToken)
            if(( valueToken.head.tokentype != TokenType.NIL ) || ( valueToken.head.tokentype != TokenType.IDENTIFIER )){
              dataTypeToken.head.value match {
                case "int"    => Try (valueToken.head.value.toInt).getOrElse(throw new Exception("Error: Value should be Integer"))
                case "bool"   => Try (valueToken.head.value.toInt).getOrElse(throw new Exception("Error: Value should be Boolean"))
                case "alpha"  => Try (valueToken.head.value.toInt).getOrElse(throw new Exception("Error: Value should be Alpha"))
                case _        => throw new Exception("Error: Invalid DataType")
              }
            }
            val (nextTokens2, right_node) = expr(valueToken)
            (nextTokens2, Declare(left_node, nextTokens.head, right_node))
      }
      case TokenType.BREAK        => {
        val right_node = new Nil(new Token(null,TokenType.NIL))
        (assignToken, Declare(left_node, nextTokens.head, right_node))
      }
      case _                      => throw new Exception("Error: Invalid Assignment Statement")
    }

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

  def identifier_statement(tokens: List[Token]): (List[Token], node) = {
    if (tokens.length>1) {
      tokens.apply(1).value match {
        case "=" => assign_statement(tokens)
        case "-" | "==" | "><" | "and" | "or" | "^" | ">" | "<" | "+" | "/" | "*" => expr(tokens)
        case _ => variable(tokens)
      }
    } else variable(tokens)
  }

  def  literal_statement(tokens: List[Token]): (List[Token], node) = {
    tokens.apply(1).value match {
      case "-" | "==" | "><" | "and" | "or" | "\\^" | ">" | "<" | "\\+" | "\\/" | "\\*" => expr(tokens)
      case _ => factor(tokens)
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
    val (nextTokens, statement_node) = statements(getNextToken(doToken))
    (nextTokens, While(expr_node, statement_node))
  }

  def print_statement(tokens : List[Token]): (List[Token], node) = {
    val (nextTokens, expr_node) = statement(getNextToken(tokens))
    (nextTokens, Print(expr_node))
  }

  def factor(tokens : List[Token]): (List[Token], node) = {

    tokens.head.tokentype match {
      case TokenType.UOP      => val(nextTokens, node) = factor(getNextToken(tokens));return (nextTokens, UnaryOp(tokens.head, node));
      case TokenType.INTEGER  => (getNextToken(tokens), Integer(tokens.head))
      case TokenType.BOOLEAN  => (getNextToken(tokens), Bool(tokens.head))
      case TokenType.ALPHA    => (getNextToken(tokens), Alpha(tokens.head))
      case TokenType.NIL      => (getNextToken(tokens), Nil(tokens.head))
      case _                  => (getNextToken(tokens), Var(tokens.head))
    }

  }

  def term(tokens : List[Token]): (List[Token], node) = {

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
    val (nextTokens, left_node) = factor(tokens);
    nextTerm(nextTokens, left_node)
  }


  def expr(tokens:List[Token]): (List[Token], node) = {

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
    val (newTokens, left_node) = term(tokens);
    nextExpr(newTokens, left_node)

  }


  def variable(tokens : List[Token]): (List[Token], node) = (getNextToken(tokens), Var(tokens.head))

  def const(tokens : List[Token]): (List[Token], node) = (getNextToken(tokens), Const(tokens.head))

  def parse(): node = {
    val (theTokens,tree) = program(lexer.lexData)
    println(tree)
    return tree
  }
}
