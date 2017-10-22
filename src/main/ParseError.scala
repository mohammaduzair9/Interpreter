package ASTBuilderParser

import LexicalAnalyzer.Token

object ParseError {

  def printError(err: String, tokens : List[Token], tokenList :List[Token]) {

    val line = getLine(tokens,tokenList,1)._1

    println("Error in line : " + line)
    println(err)
    System.exit(0)
  }

  def getLine(tokens:List[Token],tokenList: List[Token], line : Int): (Int, List[Token]) ={
      if(tokens.equals(tokenList)) (line, tokenList)
      else{
        val newline = if(tokenList.head.value.equals("\n")) line+1 else line
        getLine(tokens,tokenList.tail,newline)
      }
  }
}
