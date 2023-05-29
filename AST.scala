package zetashift.cta

import parsley.genericbridges.*
enum AST:
  case Number(value: Double)
  case Identifier(value: String)
  case Not(term: AST)
  case Equal(left: AST, right: AST)
  case NotEqual(left: AST, right: AST)
  case Add(left: AST, right: AST)
  case Subtract(left: AST, right: AST)
  case Multiply(left: AST, right: AST)
  case Divide(left: AST, right: AST)
  case Call(callee: String, arguments: Vector[AST])
  case Return(term: AST)
  case Block(statements: Vector[AST])
  case If(conditional: AST, consequence: AST, alternative: AST)
  case Function(name: String, parameters: Vector[String], body: AST)
  case Var(name: String, value: AST)
  case Assign(name: String, value: AST)
  case While(conditional: AST, body: AST)

object AST:
  object Number extends ParserBridge1[Double, Number]
  object Identifier extends ParserBridge1[String, Identifier]
  object Not extends ParserBridge1[AST, Not]
  object Equal extends ParserBridge2[AST, AST, Equal]
  object NotEqual extends ParserBridge2[AST, AST, NotEqual]
  object Add extends ParserBridge2[AST, AST, Add]
  object Subtract extends ParserBridge2[AST, AST, Subtract]
  object Multiply extends ParserBridge2[AST, AST, Multiply]
  object Divide extends ParserBridge2[AST, AST, Divide]
  object Call extends ParserBridge2[String, Vector[AST], Call]
  object Return extends ParserBridge1[AST, Return]
  object Block extends ParserBridge1[Vector[AST], Block]
  object If extends ParserBridge3[AST, AST, AST, If]
  object Function extends ParserBridge3[String, Vector[String], AST, Function]
  object Var extends ParserBridge2[String, AST, Var]
  object Assign extends ParserBridge2[String, AST, Assign]
  object While extends ParserBridge2[AST, AST, While]
