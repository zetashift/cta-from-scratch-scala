package zetashift.cta
import parsley.Parsley
import parsley.Parsley.{attempt, pure}
import parsley.character.{
  whitespace,
  string,
  item,
  endOfLine,
  digit,
  satisfy,
  stringOfMany,
  letterOrDigit,
  char
}
import parsley.combinator.{many, manyN, manyUntil, skipMany, option}
import parsley.errors.combinator.ErrorMethods
import parsley.implicits.zipped.Zipped2
import parsley.expr.chain.*

def symbol(s: String) = attempt(string(s))

val lineComment = symbol("//") *> manyUntil(item, endOfLine)
val multiLineComment = symbol("/*") *> manyUntil(item, symbol("*/"))

val comments = lineComment <|> multiLineComment

val ignored = skipMany(whitespace <|> comments).hide

def token(s: String): Parsley[String] =
  val sym = symbol(s)
  attempt(sym <* ignored)

val Function = token("function")
val If = token("if")
val Else = token("else")
val Return = token("return")
val Var = token("var")
val While = token("while")

val Comma = token(",")
val Semicolon = token(";")
val LeftParen = token("(")
val RightParen = token(")")
val LeftBrace = token("{")
val RightBrace = token("}")
val Number = digit.map(c => AST.Number(c.toDouble))

val Identifier = attempt {
  (
    satisfy(c => c.isLetter || c == '_'),
    stringOfMany(letterOrDigit | char('_'))
  ).zipped((c, s) => s"$c$s")
}
val id = Identifier.map(value => AST.Identifier(value))

val Not: Parsley[AST => AST] = token("!").map(_ => AST.Not(_))
val Equal: Parsley[(AST, AST) => AST] = token("==").map(_ => AST.Equal(_, _))
val NotEqual = token("!=").map(_ => AST.NotEqual(_, _))
val Plus = token("+").map(_ => AST.Add(_, _))
val Minus = token("-").map(_ => AST.Subtract(_, _))
val Star = token("*").map(_ => AST.Multiply(_, _))
val Slash = token("/").map(_ => AST.Divide(_, _))
val Assign = token("=").map(_ => AST.Assign(_, _))

lazy val expression: Parsley[AST] = comparison

// comparison <- sum ((EQUAL / NOT_EQUAL) sum)*
lazy val comparison: Parsley[AST] = left1(sum, Equal | NotEqual)

// sum <- product ((PLUS / MINUS) product)*
lazy val sum: Parsley[AST] = left1(product, Plus | Minus)

// product <- unary ((STAR / SLASH) unary)*
lazy val product: Parsley[AST] = left1(unary, Star | Slash)

lazy val unary: Parsley[AST] = for
  not <- option(Not)
  term <- atom
yield not match
  case Some(_) => AST.Not(term)
  case None    => term

// atom <- call / ID / Number / LeftParen expression RightParen
lazy val atom: Parsley[AST] =
  call | id | Number | (LeftParen *> expression).flatMap((e: AST) =>
    RightParen *> pure(e)
  )

// call <- ID LeftParen arguments RightParen
lazy val call: Parsley[AST] = for
  callee <- Identifier
  _ <- LeftParen
  args <- arguments
  _ <- RightParen
yield AST.Call(callee, args)

// args <- (expression (COMMA expression)*)
lazy val arguments: Parsley[Vector[AST]] =
  val result = for
    arg <- expression
    args <- many(Comma *> expression)
    _ <- RightParen
  yield Vector(arg).appendedAll(args)
  result </> Vector()

@main def tests() =
  val test1 = """!test"""
  val result1 = expression.parse(test1)
  println(result1)
