package Lexing
import scala.util.matching.Regex

case class Token(val value: String, tokentype: TokenType._type)
case class TokenInfo(tokenRegex: Regex, tokenType: TokenType._type)

class Lexer(data: String) {

  val SKIP = new TokenInfo(TokenRegex.SKIP, TokenType.SKIP)
  val BREAK = new TokenInfo(TokenRegex.BREAK, TokenType.BREAK);
  val PRINT = new TokenInfo(TokenRegex.PRINT, TokenType.PRINT);
  val VAR_TYPE = new TokenInfo(TokenRegex.VAR_TYPE, TokenType.VAR_TYPE);
  val DATA_TYPE = new TokenInfo(TokenRegex.DATA_TYPE, TokenType.DATA_TYPE);
  val CONST_TYPE = new TokenInfo(TokenRegex.CONST_TYPE, TokenType.CONST_TYPE)
  val IF = new TokenInfo(TokenRegex.IF, TokenType.IF);
  val THEN = new TokenInfo(TokenRegex.THEN, TokenType.THEN);
  val ELSE = new TokenInfo(TokenRegex.ELSE, TokenType.ELSE);
  val WHILE = new TokenInfo(TokenRegex.WHILE, TokenType.WHILE);
  val DO = new TokenInfo(TokenRegex.DO, TokenType.DO);
  val UOP = new TokenInfo(TokenRegex.UOP, TokenType.UOP);
  val PLUS = new TokenInfo(TokenRegex.PLUS, TokenType.PLUS)
  val MUL = new TokenInfo(TokenRegex.MUL, TokenType.MUL)
  val DIV = new TokenInfo(TokenRegex.DIV, TokenType.DIV)
  val BOP = new TokenInfo(TokenRegex.BOP, TokenType.BOP);
  val ASSIGNMENT = new TokenInfo(TokenRegex.ASSIGNMENT, TokenType.ASSIGNMENT);
  val COLON = new TokenInfo(TokenRegex.COLON, TokenType.COLON);
  val INTEGER = new TokenInfo(TokenRegex.INTEGER, TokenType.INTEGER);
  val BOOLEAN = new TokenInfo(TokenRegex.BOOLEAN, TokenType.BOOLEAN);
  val ALPHA = new TokenInfo(TokenRegex.ALPHA, TokenType.ALPHA);
  val NIL = new TokenInfo(TokenRegex.NIL, TokenType.NIL);
  val IDENTIFIER = new TokenInfo(TokenRegex.IDENTIFIER,TokenType.IDENTIFIER);
//  val SPACE = new TokenInfo(TokenRegex.SPACE, TokenType.SPACE);

  val lexList  = List(SKIP, BREAK, PRINT, VAR_TYPE, DATA_TYPE, CONST_TYPE, IF, THEN, ELSE, WHILE, DO, UOP , PLUS, MUL, DIV, BOP,
                      ASSIGNMENT, COLON, INTEGER, BOOLEAN, ALPHA, NIL, IDENTIFIER);

  val lexData = lex(data)

  def lex(data: String): List[Token] = {
    val newData = data.trim()
    if (!newData.isEmpty){
      val (token, before, after) = getToken(newData, lexList)
      List(token).::: (lex(before)):::lex(after)
    }
    else{
      List()
    }
  }

  def getToken(data: String, lexList: List[TokenInfo]) : (Token, String, String) = {
      val nextToken = lexList.apply(0)
      val matcher= nextToken.tokenRegex.pattern.matcher(data)
      if (matcher.find()) {
        val preTokenData = data.substring(0, matcher.start())
        val TokenFound = new Token(data.substring(matcher.start(), matcher.end()),nextToken.tokenType)
        val postTokenData = data.substring(matcher.end())

        (TokenFound, preTokenData,postTokenData)
      }
      else
        getToken(data, lexList.tail)
  }

  def getNextToken(pos: Int) : (Int, Token) = {
    if(pos < lexData.length )
      (pos+1 , lexData.apply(pos))
    else
      (pos , new Token("EOF",null))

  }


}
