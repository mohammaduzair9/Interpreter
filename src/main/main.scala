import Lexing._
import Parsing._

object main {
  def main(args: Array[String]) {
    //val codeFile = Source.fromFile("examples/if.appp")
    val str:String = "var x:int = 10; var y:int;"
    val lexer: Lexer = new Lexer(str)
    //tokenizer.tokenize(str).map(f=> println(f.getToken()+" "+f.getType()) )
    val parser: Parser = new Parser(lexer)

//    val id_value = new value("var", "int","x", 1)
 //   val var_table = Map("x"->id_value).withDefaultValue((new value("null","null","null",0)))

    //val interpreter = new Interpreter(parser, var_table)
    parser.parse()
  }
}
