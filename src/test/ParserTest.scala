import LexicalAnalyzer._
import ASTBuilderParser._
import ASTEvaluator._
import org.scalatest.{FunSuite, Matchers}


class ParserTest extends FunSuite with Matchers {

  test("Parser Test A"){

    val var_table = Map[String,identifier]().withDefaultValue(new identifier("null","null",0))
    val str:String = "var x:bool = tt; print x"
    val tokenizer: Lexer = new Lexer(str)
    val parser: Parser = new Parser(tokenizer)
    parser.parse().toString should be ("Block(List(Declare(Var(Token(x,IDENTIFIER)),Token(:,COLON),Bool(Token(tt,BOOLEAN))), Print(Var(Token(x,IDENTIFIER)))))")

  }

  test("Parser Test B"){

    val var_table = Map[String,identifier]().withDefaultValue(new identifier("null","null",0))
    val str:String = "var x:bool = tt; print x"
    val tokenizer: Lexer = new Lexer(str)
    val parser: Parser = new Parser(tokenizer)
    parser.getNextToken(tokenizer.lexData) should be (tokenizer.lexData.tail)

  }

}
