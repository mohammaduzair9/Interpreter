package ASTEvaluator

import java.awt.image.LookupTable

import LexicalAnalyzer._
import ASTBuilderParser.AST._
import ASTBuilderParser.Parser

import scala.util.Try

case class identifier( varType: String, dataType: String , value: Int)

class Evaluator(parser: Parser, lookupTable: Map[String, identifier]) {

  type lookupTable = Map[String, identifier]
  def evalNode(node: node, lookupTable: lookupTable): (Int,lookupTable) = {


    if      (node.isInstanceOf[BinaryOp])   evalNodeBinOp(node.asInstanceOf[BinaryOp], lookupTable)
    else if (node.isInstanceOf[UnaryOp])    evalNodeUnaryOp(node.asInstanceOf[UnaryOp], lookupTable)
    else if (node.isInstanceOf[Integer])    evalNodeInt(node.asInstanceOf[Integer], lookupTable)
    else if (node.isInstanceOf[Bool])       evalNodeBool(node.asInstanceOf[Bool], lookupTable)
    else if (node.isInstanceOf[Alpha])      evalNodeAlpha(node.asInstanceOf[Alpha], lookupTable)
    else if (node.isInstanceOf[Declare])    evalNodeVarDec(node.asInstanceOf[Declare], lookupTable)
    else if (node.isInstanceOf[Assign])     evalNodeAssign(node.asInstanceOf[Assign], lookupTable)
    else if (node.isInstanceOf[Var])        evalNodeVar(node.asInstanceOf[Var],lookupTable)
    else if (node.isInstanceOf[IfElse])     evalNodeIf(node.asInstanceOf[IfElse], lookupTable)
    else if (node.isInstanceOf[While])      evalNodeWhile(node.asInstanceOf[While], lookupTable)
    else if (node.isInstanceOf[Print])      evalNodePrint(node.asInstanceOf[Print], lookupTable)
    else if (node.isInstanceOf[Nil])        evalNodeNil(node.asInstanceOf[Nil], lookupTable)
    else throw new Exception("Invalid node")


  }

  def evalAST(tree: Block, lookupTable: lookupTable) = {

    val (result, finalTable) = evalNode(tree.Statements.head, lookupTable)
    evalNodes(tree.Statements.tail, finalTable)

    def evalNodes(nodes: List[node], lookupTable: lookupTable) {
      if (!nodes.isEmpty) evalNodes(nodes.tail, evalNode(nodes.head, lookupTable)._2)
    }

  }

  def evalNodeBinOp(node: BinaryOp, lookupTable: lookupTable): (Int, Map[String,identifier]) = {

    if (node.LeftOp.isInstanceOf[Alpha] && node.RightOp.isInstanceOf[Alpha]){
      println(node)
      throw new Exception("Cant Perform Binary Operation on Alpha")
    }
    else {
      val leftResult = evalNode(node.LeftOp, lookupTable)._1
      val rightResult = evalNode(node.RightOp, lookupTable)._1
      node.Token.tokentype match {
        case TokenType.PLUS   => (leftResult + rightResult, lookupTable)
        case TokenType.MUL    => (leftResult * rightResult, lookupTable)
        case TokenType.DIV    => (leftResult / rightResult, lookupTable)
        case TokenType.BOP    => {
          node.Token.value match {
            case "^"    => (Math.pow(leftResult, rightResult).toInt, lookupTable)
            case "=="   => if (leftResult   ==  rightResult) (1, lookupTable) else (0, lookupTable)
            case "><"   => if (leftResult   !=  rightResult) (1, lookupTable) else (0, lookupTable)
            case ">"    => if (leftResult   >   rightResult) (1, lookupTable) else (0, lookupTable)
            case "<"    => if (leftResult   <   rightResult) (1, lookupTable) else (0, lookupTable)
            case "and"  => (leftResult * rightResult, lookupTable)
            case "or"   => (leftResult + rightResult, lookupTable)
          }
        }
        case _ => throw new Exception("No such Operation available")
      }
    }
  }

  def evalNodeUnaryOp(node: UnaryOp, lookupTable: lookupTable): (Int, Map[String,identifier]) = {

    val result = evalNode(node.Op, lookupTable)._1
    if (!node.Op.isInstanceOf[Bool]) (-result, lookupTable)
    else{
      if (result != 0) (0,lookupTable)
      else (1, lookupTable)
    }
  }

  def evalNodeInt(node: Integer, lookupTable: lookupTable):(Int, Map[String,identifier]) = (node.Token.value.toInt, lookupTable)

  def evalNodeBool(node: Bool, lookupTable: lookupTable): (Int, Map[String,identifier]) = {

    if(node.Token.value=="tt") (1,lookupTable)
    else if(node.Token.value=="ff") (0,lookupTable)
    else throw new Exception("Invalid bool value")
  }

  def evalNodeAlpha(node: Alpha, lookupTable: lookupTable): (Int, Map[String,identifier]) = {

    val noQuoteString = node.Token.value.substring(1, node.Token.value.length()-1)
    print(noQuoteString)
    (1, lookupTable)
  }

  def evalNodeVarDec(node: Declare, lookupTable: lookupTable): (Int, Map[String,identifier]) = {

    val varName = if (node.Name.isInstanceOf[Var]) node.Name.asInstanceOf[Var].Token.value else node.Name.asInstanceOf[Const].Token.value
    val varIdentifier = lookupTable(varName)

    if (!varIdentifier.varType.equals("null")) throw new Exception("Variable " + varName + " is already declared")
    else {
      val varType = if (node.Name.isInstanceOf[Var]) "var" else "const"
      val dataType = if(node.Value.isInstanceOf[Integer]) "int" else if(node.Value.isInstanceOf[Bool]) "bool" else "alpha"
      val value = evalNode(node.Value, lookupTable)._1
      val newIdentifier =  new identifier(varType, dataType, value)
      val newLookupTable = lookupTable + (varName -> newIdentifier)
      (1, newLookupTable)
    }
  }


  def evalNodeAssign(node: Assign, lookupTable: Map[String,identifier]): (Int,Map[String,identifier]) = {

    val varName = node.Name.asInstanceOf[Var].Token.value
    val varIdentifier = lookupTable(varName)
    if (varIdentifier.varType.equals("null")) throw new Exception("Error: Variable not declared")
    else if (varIdentifier.varType.equals("const")) throw new Exception("Error: Const Value cannot be updated")
    else {
      val dataType = varIdentifier.dataType
      val value = evalNode(node.Value, lookupTable)._1
      dataType match {
        case "int"    => Try (value.toInt).getOrElse(throw new Exception("Error: Value should be Integer"))
        case "bool"   => {

          if(!(value==1 || value==0))
            throw new Exception("Error: Value should be Boolean")
        }
        case "alpha"  => {
          if (Try (value.toInt).isSuccess)
            (throw new Exception("Error: Value should be Alpha"))
          else if (Try (value.toString).isFailure)
            (throw new Exception("Error: Value should be Alpha"))
        }
        case _        => throw new Exception(dataType + " type variable cannot be assigned a " + value.getClass.getSimpleName)
      }

      val newIdentifier =  (new identifier(varIdentifier.varType, varIdentifier.dataType, value))
      val new_lookupTable = lookupTable.+(varName -> newIdentifier)
      (1, new_lookupTable)

    }
  }

  def evalNodeVar(node: Var, var_table: lookupTable): (Int, lookupTable) = {
      if (!var_table(node.Token.value).varType.equals("null")){
        val int_value = var_table(node.Token.value).value
        return (int_value, var_table)
      }
      else throw new Exception("Error: variable \""+node.Token.tokentype+"\" not found")
  }

  def evalNodeIf(node: IfElse, lookupTable: lookupTable):(Int, Map[String,identifier]) = {

    if (evalNode(node.If, lookupTable)._1 != 0)
          (evalNode(node.Then, lookupTable)._1, lookupTable)
    else  (evalNode(node.Else, lookupTable)._1, lookupTable)
  }

  def evalNodeWhile(node: While, lookupTable: lookupTable):(Int, Map[String,identifier]) = {


    def While(acc : Int, lookupTable : lookupTable ): (Int, lookupTable)={

      if (evalNode(node.While ,lookupTable)._1 == 0) (acc,lookupTable)
      else {
        def Do(acc: Int, lookupTable: Map[String, identifier], statements: List[node]): (Int, lookupTable) = {

          if (statements.isEmpty) (acc, lookupTable)
          else {
            val (answer, new_table) = evalNode(statements.head, lookupTable)
            Do(answer, new_table, statements.tail)
          }
        }
        val (answer, new_lookupTable) = Do(acc, lookupTable, node.Do)
        While(answer, new_lookupTable)
      }
    }
    While(0,lookupTable)
  }

  def evalNodePrint( node : Print, lookupTable : lookupTable):(Int, Map[String,identifier]) = {
    val answer = evalNode(node.Value, lookupTable)._1
    if(node.Value.isInstanceOf[Var]) {
      val mytype = lookupTable(node.Value.asInstanceOf[Var].Token.value)
      if(mytype.dataType.equals("bool")){
          if(answer==1) println("tt")
          if(answer==0) println("ff")
      }
      else println(answer)
    }
    else println(answer)
    return (1, lookupTable)
   }


  def evalNodeNil(node: node, lookupTable: lookupTable) = {
    (0, lookupTable)
  }

  def interpret() :Boolean = {
    val tree = parser.parse()
    evalAST(tree.asInstanceOf[Block], lookupTable)
    return true
  }
}
