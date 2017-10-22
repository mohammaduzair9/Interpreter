import LexicalAnalyzer._
import ASTBuilderParser._
import ASTEvaluator._
import org.scalatest.{FunSuite, Matchers}

class EvalTest extends FunSuite with Matchers {

  test("Program Test 1"){

    val var_table = Map[String,identifier]().withDefaultValue(new identifier("null","null",0))
    val str:String = "var x:int; const y:int=2; print x; print y "
    val tokenizer: Lexer = new Lexer(str)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Evaluator(parser, var_table)
    interpreter.interpret() should be (true)

  }

  test("Program Test 2"){

    val var_table = Map[String,identifier]().withDefaultValue(new identifier("null","null",0))
    val str:String = "var x:int = 10; var y:int = 0; while x>y do y = y+1; print y; "
    val tokenizer: Lexer = new Lexer(str)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Evaluator(parser, var_table)
    interpreter.interpret() should be (true)

  }

  test("Program Test 3"){

    val var_table = Map[String,identifier]().withDefaultValue(new identifier("null","null",0))
    val str:String = "var x:int = 10; \nconst y:int = 9;  \nif x>y then y = y+1 else y=y+1; "
    val tokenizer: Lexer = new Lexer(str)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Evaluator(parser, var_table)
    an [Exception] should be thrownBy interpreter.interpret()

  }

  test("Program Test 4"){

    val var_table = Map[String,identifier]().withDefaultValue(new identifier("null","null",0))
    val str:String = "var x:bool = tt; if x then print \"true\" else print \"false\" ; "
    val tokenizer: Lexer = new Lexer(str)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Evaluator(parser, var_table)
    interpreter.interpret() should be (true)

  }




}
