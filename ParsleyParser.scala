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
  char,
  stringOfSome
}
import parsley.lift.lift2
import parsley.combinator.{sepBy, many, manyN, manyUntil, skipMany, option}
import parsley.errors.combinator.ErrorMethods
import parsley.implicits.zipped.Zipped2
import parsley.expr.chain.*
import parsley.debug.*

def symbol(s: String) = attempt(string(s))

val lineComment = symbol("//") *> manyUntil(item, endOfLine)
val multiLineComment = symbol("/*") *> manyUntil(item, symbol("*/"))

val comments = lineComment <|> multiLineComment

val ignored = skipMany(whitespace <|> comments).hide

def token(s: String): Parsley[Unit] = symbol(s) *> ignored

val Function = token("function")
val If = token("if")
val Else = token("else")
val Return = token("return")
val Var = token("var")
val While = token("while")

val Comma = token(",")
val Semicolon = token(";")
val LeftParen: Parsley[Unit] = token("(")
val RightParen = token(")")
val LeftBrace = token("{")
val RightBrace = token("}")
val Number = stringOfSome(digit).map(n => AST.Number(n.toDouble))

val Identifier = attempt {
  (
    satisfy(c => c.isLetter || c == '_'),
    stringOfMany(letterOrDigit | char('_'))
  ).zipped((c, s) => s"$c$s")
}
val id: Parsley[AST] = AST.Identifier(Identifier)

val Not: Parsley[AST => AST] = AST.Not <# token("!")
val Equal: Parsley[(AST, AST) => AST] = AST.Equal <# token("==")
val NotEqual: Parsley[(AST, AST) => AST] = AST.NotEqual <# token("!=")

val Plus: Parsley[(AST, AST) => AST] = AST.Add <# token("+")
val Minus: Parsley[(AST, AST) => AST] = AST.Subtract <# token("-")
val Star: Parsley[(AST, AST) => AST] = AST.Multiply <# token("*")
val Slash: Parsley[(AST, AST) => AST] = AST.Divide <# token("/")
val Assign: Parsley[(String, AST) => AST] = AST.Assign <# token("=")

lazy val expression: Parsley[AST] = comparison

// comparison <- sum ((EQUAL / NOT_EQUAL) sum)*
lazy val comparison: Parsley[AST] = left1(sum, Equal | NotEqual)

// sum <- product ((PLUS / MINUS) product)*
lazy val sum: Parsley[AST] = left1(product, Plus | Minus)

// product <- unary ((STAR / SLASH) unary)*
lazy val product: Parsley[AST] = left1(unary, Star | Slash)

lazy val unary: Parsley[AST] = prefix(AST.Not <# Not, atom)

/** In this case, attempt is saying that its argument can either be parsed
  * entirely, or not at all with no input consumed if it fails.
  */
// atom <- call / ID / Number / LeftParen expression RightParen
lazy val atom: Parsley[AST] =
  val callOrId =
    (Identifier, option(LeftParen *> arguments <* RightParen)).zipped {
      case (id, Some(args)) => AST.Call(id, args)
      case (id, None)       => AST.Identifier(id)
    }
  callOrId | Number | (LeftParen *> expression <* RightParen)

// call <- ID LeftParen arguments RightParen
lazy val call: Parsley[AST] =
  AST.Call(attempt(Identifier <* LeftParen), arguments <* RightParen)

// args <- (expression (COMMA expression)*)
lazy val arguments: Parsley[Vector[AST]] = sepBy(expression, Comma).map(_.toVector)


@main def tests() =
  val test1 = """(1 + 3)"""
  val result1 = expression.parse(test1)
  println(result1)
