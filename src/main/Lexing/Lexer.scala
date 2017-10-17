import scala.util.matching.Regex

case class Token(val value: String, tokentype: TokenType._type)
case class TokenData(pattern: Regex, tokentype: TokenType._type)

class Lexer(userData: String) {

  val _space = TokenData(TokenRegex.SPACE, TokenType.SPACE);
  val _break = TokenData(TokenRegex.BREAK, TokenType.BREAK);
  val _int_literal = TokenData(TokenRegex.INTEGER, TokenType.INTEGER);
  val _bool_literal = TokenData(TokenRegex.BOOLEAN, TokenType.BOOLEAN);
  val _alpha_literal = TokenData(TokenRegex.ALPHA, TokenType.ALPHA);
  val _bop = TokenData(TokenRegex.BOP, TokenType.BOP);
  val _uop = TokenData(TokenRegex.UOP, TokenType.UOP);
  val _assign_op = TokenData(TokenRegex.ASSIGNMENT, TokenType.ASSIGNMENT);
  val _colon = TokenData(TokenRegex.COLON, TokenType.COLON);
  val _while = TokenData(TokenRegex.WHILE, TokenType.WHILE);
  val _do = TokenData(TokenRegex.DO, TokenType.DO);
  val _if = TokenData(TokenRegex.IF, TokenType.IF);
  val _then = TokenData(TokenRegex.THEN, TokenType.THEN);
  val _else = TokenData(TokenRegex.ELSE, TokenType.ELSE);
  val _nil = TokenData(TokenRegex.NIL, TokenType.NIL);
  val _print = TokenData(TokenRegex.PRINT, TokenType.PRINT);
  val _datatype = TokenData(TokenRegex.DATA_TYPE, TokenType.DATA_TYPE);
  val _identifier = TokenData(TokenRegex.IDENTIFIER,TokenType.IDENTIFIER);
  val _vartype = TokenData(TokenRegex.VAR_TYPE, TokenType.VAR_TYPE);

  val lexList  = List(_break,_vartype, _print, _while, _do, _if, _then, _else, _colon, _datatype, _assign_op,_int_literal, _uop, _bop, _alpha_literal, _bool_literal , _identifier );
  val lexedUserData = lex(userData)

  def lex(userData: String): List[Token] = {
    if (userData.trim().isEmpty)
      List()
    else {
      val (before,token,after) = findToken(userData.trim(), lexList)
      println("before: "+before+" | token : "+ token.value +" | after: "+after)
      lex(before) ::: List(token) ::: lex(after)
    }
  }

  def findToken(userData: String, lexList: List[TokenData]) : (String, Token, String) = {
    if (lexList.isEmpty) throw new Exception("No match found")
    val TokenRegex = lexList.head.pattern
    val tokentype = lexList.head.tokentype

    val matcher = TokenRegex.pattern.matcher(userData)
    if (matcher.find()){
      (
        userData.substring(0,matcher.start()),
        new Token(userData.substring(matcher.start(),matcher.end()), tokentype),
        userData.substring(matcher.end())
      )
    }else {
      findToken(userData, lexList.tail)
    }
  }

  def getNextToken(pos: Int) : (Int , Token)={
    if(pos < lexedUserData.length )
      (pos+1 , lexedUserData.apply(pos))
    else
      (-1 , null)
  }


}
