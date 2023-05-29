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
import cats.syntax.all.*
import parsley.cats.instances.*

def symbol(s: String) = attempt(string(s))

val lineComment = symbol("//") *> manyUntil(item, endOfLine)
val multiLineComment = symbol("/*") *> manyUntil(item, symbol("*/"))

val comments = lineComment <|> multiLineComment

val ignored = skipMany(whitespace <|> comments).hide

def lexeme[A](p: Parsley[A]): Parsley[A] = p <* ignored
def token(s: String): Parsley[Unit] = lexeme(attempt(symbol(s)).void)

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
val Number = lexeme(stringOfSome(digit).map(n => AST.Number(n.toDouble)))

val Identifier = lexeme(attempt {
  (
    satisfy(c => c.isLetter || c == '_'),
    stringOfMany(letterOrDigit | char('_'))
  ).zipped((c, s) => s"$c$s")
})

val id: Parsley[AST] = AST.Identifier(Identifier)

val Not: Parsley[Unit] = token("!")
val Equal: Parsley[(AST, AST) => AST] = AST.Equal <# token("==")
val NotEqual: Parsley[(AST, AST) => AST] = AST.NotEqual <# token("!=")

val Plus: Parsley[(AST, AST) => AST] = AST.Add <# token("+")
val Minus: Parsley[(AST, AST) => AST] = AST.Subtract <# token("-")
val Star: Parsley[(AST, AST) => AST] = AST.Multiply <# token("*")
val Slash: Parsley[(AST, AST) => AST] = AST.Divide <# token("/")
val Assign: Parsley[Unit] = token("=")

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
lazy val arguments: Parsley[Vector[AST]] =
  sepBy(expression, Comma).map(_.toVector)

lazy val statement: Parsley[AST] =
  returnStatement | ifStatement | whileStatement | functionStatement | varStatement | assignStatement | expressionStatement

// returnStatement <- RETURN expression SEMICOLON
lazy val returnStatement: Parsley[AST] =
  (Return *> expression <* Semicolon).map(e => AST.Return(e))

lazy val expressionStatement: Parsley[AST] =
  expression <* Semicolon

lazy val ifStatement: Parsley[AST] =
  (
    If *> LeftParen *> expression,
    RightParen *> statement,
    Else *> statement
  ).mapN { (conditional, consequence, alternative) =>
    AST.If(conditional, consequence, alternative)
  }

// blockStatement <- LEFT_BRACE statement* RIGHT_BRACE
lazy val blockStatement: Parsley[AST] =
  (LeftBrace *> many(statement) <* RightBrace).map { statements =>
    val v = statements.toVector
    AST.Block(v)
  }

// whileStatement <- WHILE LEFT_PAREN expression RIGHT_PAREN statement
lazy val whileStatement: Parsley[AST] =
  (
    While *> LeftParen *> expression,
    RightParen *> statement
  ).mapN { (conditional, body) => AST.While(conditional, body) }

// varStatement <- VAR ID ASSIGN expression SEMICOLON
lazy val varStatement: Parsley[AST] =
  (Var *> Identifier, Assign *> expression <* Semicolon).mapN { (name, value) =>
    AST.Var(name, value)
  }

// assignmentStatement <- ID ASSIGN expression SEMICOLON
lazy val assignStatement: Parsley[AST] =
  (
    Identifier,
    Assign *> expression <* Semicolon,
  ).mapN { (name, value) => AST.Assign(name, value) }

// parameters <- (ID (COMMA ID)*)?
lazy val parameters: Parsley[Vector[String]] =
  (Identifier, sepBy(Identifier, Comma)).mapN { (id, params) =>
    Vector(id).appendedAll(params.toVector)
  } </> Vector()

// functionStatement <- FUNCTION ID LEFT_PAREN parameters RIGHT_PAREN blockstatement
lazy val functionStatement: Parsley[AST] =
  (
    Function *> Identifier,
    LeftParen *> parameters,
    RightParen *> blockStatement
  ).mapN { (name, params, block) => AST.Function(name, params, block) }

// We need to be able to parse more than one statement
val parser: Parsley[AST] =
  (ignored *> many(statement)).map(statements => AST.Block(statements.toVector))

@main def tests() =
  val test1 = """function sdfs()"""
  val stringD = """
    function factorial(n) {
      var result = 1;
      while (n != 1) {
        result = result * n;
        n = n - 1;
      }
      return result;
    }
  """

  val result1 = parser.parse(stringD)
  println(result1)
