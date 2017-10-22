import LexicalAnalyzer._
import ASTBuilderParser._
import ASTEvaluator._
import org.scalatest.{FunSuite, Matchers}

class RuleTest extends FunSuite with Matchers {

  test("Bool Test"){

    val var_table = Map[String,identifier]().withDefaultValue(new identifier("null","null",0))
    val str:String = "var x:bool = tt; print x"
    val tokenizer: Lexer = new Lexer(str)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Evaluator(parser, var_table)
    interpreter.interpret() should be (true)

  }

  test("Int Test"){

    val var_table = Map[String,identifier]().withDefaultValue(new identifier("null","null",0))
    val str:String = "var x:int = 5;"
    val tokenizer: Lexer = new Lexer(str)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Evaluator(parser, var_table)
    interpreter.interpret() should be (true)

  }

  test("Alpha Test"){

    val var_table = Map[String,identifier]().withDefaultValue(new identifier("null","null",0))
    val str:String = "var x:alpha = \"abc\";"
    val tokenizer: Lexer = new Lexer(str)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Evaluator(parser, var_table)
    interpreter.interpret() should be (true)

  }

  test("Plus Test"){

    val var_table = Map[String,identifier]().withDefaultValue(new identifier("null","null",0))
    val str:String = "var x:int = 14; x=x+14;"
    val tokenizer: Lexer = new Lexer(str)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Evaluator(parser, var_table)
    interpreter.interpret() should be (true)

  }

  test("Multiply Test"){

    val var_table = Map[String,identifier]().withDefaultValue(new identifier("null","null",0))
    val str:String = "var x:int = 14; x=x*14;"
    val tokenizer: Lexer = new Lexer(str)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Evaluator(parser, var_table)
    interpreter.interpret() should be (true)

  }

  test("IF Test"){

    val var_table = Map[String,identifier]().withDefaultValue(new identifier("null","null",0))
    val str:String = "var x:int = 14; if x><10 then x else x;"
    val tokenizer: Lexer = new Lexer(str)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Evaluator(parser, var_table)
    interpreter.interpret() should be (true)

  }

  test("BOP Test"){

    val var_table = Map[String,identifier]().withDefaultValue(new identifier("null","null",0))
    val str:String = "2><4"
    val tokenizer: Lexer = new Lexer(str)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Evaluator(parser, var_table)
    interpreter.interpret() should be (true)

  }


  test("UOP Test"){

    val var_table = Map[String,identifier]().withDefaultValue(new identifier("null","null",0))
    val str:String = "var x:bool = tt ; if not x then x else x"
    val tokenizer: Lexer = new Lexer(str)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Evaluator(parser, var_table)
    interpreter.interpret() should be (true)

  }

  test("Print Test"){

    val var_table = Map[String,identifier]().withDefaultValue(new identifier("null","null",0))
    val str:String = "var x:bool = tt ; print x"
    val tokenizer: Lexer = new Lexer(str)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Evaluator(parser, var_table)
    interpreter.interpret() should be (true)

  }

  test("WHILE Test"){

    val var_table = Map[String,identifier]().withDefaultValue(new identifier("null","null",0))
    val str:String = "var x:int = 5 ; while x < 10 do x=x+1"
    val tokenizer: Lexer = new Lexer(str)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Evaluator(parser, var_table)
    interpreter.interpret() should be (true)

  }



}
