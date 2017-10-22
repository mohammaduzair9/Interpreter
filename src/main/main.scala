import LexicalAnalyzer._
import ASTBuilderParser._
import ASTEvaluator._

object main {
  def main(args: Array[String]) {
    val var_table = Map[String,identifier]().withDefaultValue(new identifier("null","null",0))

    val str:String = "var x:int = 5; while x><10 do x=x+1;printx; "
    val tokenizer: Lexer = new Lexer(str)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Evaluator(parser, var_table)
    interpreter.interpret()
  }
}
