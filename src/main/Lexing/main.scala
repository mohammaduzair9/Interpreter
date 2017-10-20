package Lexing

object main {
  def main(args: Array[String]) {
    //val codeFile = Source.fromFile("examples/if.appp")
    val sourceCode = "var x: alpha = \"testing\"";
    println(sourceCode)
    val lexer: Lexer = new Lexer(sourceCode);
    lexer.lex(sourceCode).map(token => println(token.value + " : " + token.tokentype))
    //println(lexer.getNextToken(2))
  }
}
