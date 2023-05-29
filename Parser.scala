package zetashift.cta.oldimpl
import scala.util.matching.Regex
import cats.syntax.all.*
import cats.Applicative
import zetashift.cta.AST

case class Source(input: String, index: Int):
  def matches(regexp: Regex): ParseResult[String] =
    val matcher = regexp.pattern.matcher(input).region(index, input.length())
    if (matcher.lookingAt()) then
      val matchedString = matcher.group()
      val newSource = new Source(input, index + matchedString.length())
      ParseResult.Success(matchedString, newSource)
    else ParseResult.Failed

enum ParseResult[+A]:
  case Success(value: A, source: Source)
  case Failed

  def getValue: Option[A] = this match
    case Success(value, source) => Some(value)
    case Failed                 => None

trait Parser[+A]:
  def parse(source: Source): ParseResult[A]

  /** A helper method that parses a string to completion. Helpful for testing
    * and using the resulting parsers.
    * @param input
    *   the string to parse
    * @return
    *   the parsed value obtained from parsing the input, or a string containing
    *   the error message
    */
  def parseStringToCompletion(input: String): Either[String, A] =
    val source = Source(input, 0)
    val parsed = this.parse(source)
    parsed match
      case ParseResult.Success(value, source) =>
        if source.index == source.input.length() then Right(value)
        else Left(s"Parse error at index ${source.index}")
      case ParseResult.Failed => Left("Parse error at index 0")

object Parser:
  /** Make our parser an applicative for some goodies * */
  given Applicative[Parser] with
    def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] =
      ff.flatMap(f => fa.flatMap(a => pure(f(a))))

    def pure[A](a: A): Parser[A] = constant(a)

  def fromFunction[A](action: Source => ParseResult[A]): Parser[A] =
    new Parser:
      override def parse(source: Source): ParseResult[A] = action(source)

  /** A combinator that creates a Parser that matches a regexp.
    *
    * @param regexp
    *   the regex pattern that needs to be matches
    * @return
    *   A Parser that parses a source, matching on the regex
    */
  def regex(regexp: Regex): Parser[String] =
    Parser.fromFunction(source => source.matches(regexp))

  /** A Parser that always succeeds, returning a constant value and does not
    * advance the source.
    *
    * @param value
    *   value to return
    * @return
    *   A Parser that succeeds and holds the value with the source unchanged
    */
  def constant[A](value: A): Parser[A] =
    Parser.fromFunction(source => ParseResult.Success(value, source))

  /** A parser that just throws an error, the exception is not handled and is
    * expected to terminate the program. TODO: a better version would be to
    * inspect the source and convert the source index into a line-column pair
    * and display it nicely with the line and context.
    */
  def throwError[A](message: String): Parser[A] =
    Parser.fromFunction(source => throw Exception(message))

  /** A combinator that handles repetition
    * @param parser
    *   the Parser to repeat zero or more times
    * @return
    *   A Parser that contains a vector of results
    */
  def zeroOrMore[A](parser: Parser[A]): Parser[Vector[A]] =
    (parser, zeroOrMore(parser)).mapN((_ +: _)) | constant(Vector())

  def maybe[A, B](parser: Parser[A]): Parser[Option[A]] =
    val fn: Source => ParseResult[Option[A]] = source =>
      val result = parser.parse(source)
      result match
        case ParseResult.Success(value, newSource) =>
          ParseResult.Success(Some(value), newSource)
        case ParseResult.Failed => ParseResult.Success(None, source)

    Parser.fromFunction(source => fn(source))

  extension [A](p: Parser[A])
    /* Select between two or more alternative parser choices
     *
     * @param pb
     *   the other parser that will be tried
     * @return A parser that will try out `p` or `pb` if `p` fails
     */
    infix def |(pb: Parser[A]): Parser[A] =
      def parser: Source => ParseResult[A] = source =>
        val result = p.parse(source)
        result match
          case ParseResult.Success(value, newSource) =>
            ParseResult.Success(value, newSource)
          case ParseResult.Failed =>
            pb.parse(source)

      Parser.fromFunction(parser)

    /** Flatmap for our Parser.
      *
      * It binds a value that is being parsed to a name
      */
    def flatMap[B](fn: A => Parser[B]): Parser[B] =
      def parser = (source: Source) =>
        p.parse(source) match
          case ParseResult.Success(value, newSource) =>
            fn(value).parse(newSource)
          case ParseResult.Failed => ParseResult.Failed
      Parser.fromFunction(parser)

    /** Creates a sequence of parsers, but without binding a value to a name
      * @param pb
      *   The other parser to sequence
      */
    def and[B](pb: Parser[B]): Parser[B] = p.flatMap(_ => pb)

    /** Bind a name and return a constant parser
      * @param B
      *   type of the value that the parser will contain
      * @param fn
      * @return
      *   A Parser that contains `B`
      */
    def map[B](fn: A => B): Parser[B] =
      p.flatMap(value => Parser.constant(fn(value)))

/** Implement parsers that parse our AST
  */
object ASTParser:
  import Parser.*

  val whitespace = regex("""[ \n\r\t]+""".r)
  val comments = regex("""//.*""".r).|(regex("""/\\*.*?\\*/""".r))
  val ignored = zeroOrMore(whitespace | comments)
  val token: Regex => Parser[String] = (pattern: Regex) =>
    regex(pattern).flatMap(value => ignored.and(constant(value)))

  val Function: Parser[String] = token("""\bfunction\b""".r)
  val If = token("""\bif\b""".r)
  val Else = token("""\belse\b""".r)
  val Return = token("""\breturn\b""".r)
  val Var = token("""\bvar\b""".r)
  val While = token("""\bwhile\b""".r)

  val Comma = token("""[,]""".r)
  val Semicolon = token(""";""".r)
  val LeftParen = token("""[(]""".r)
  val RightParen = token("""[)]""".r)
  val LeftBrace = token("""[{]""".r)
  val RightBrace = token("""[}]""".r)

  val Number: Parser[AST] =
    token("""[0-9]+""".r).map(digits => AST.Number(digits.toDouble))

  val Identifier: Parser[String] = token("""[a-zA-Z_][a-zA-Z0-9_]*""".r)
  val id: Parser[AST] = Identifier.map(value => AST.Identifier(value))
  val Not: Parser[AST => AST] = token("""!""".r).map(_ => AST.Not(_))

  val Equal: Parser[(AST, AST) => AST] =
    token("""==""".r).map(_ => AST.Equal(_, _))

  val NotEqual: Parser[(AST, AST) => AST] =
    token("""!=""".r).map(_ => AST.NotEqual(_, _))

  val Plus: Parser[(AST, AST) => AST] =
    token("""[+]""".r).map(_ => AST.Add(_, _))

  val Minus: Parser[(AST, AST) => AST] =
    token("""[-]""".r).map(_ => AST.Subtract(_, _))

  val Star: Parser[(AST, AST) => AST] =
    token("""[*]""".r).map(_ => AST.Multiply(_, _))

  val Slash: Parser[(AST, AST) => AST] =
    token("""[/]""".r).map(_ => AST.Divide(_, _))

  val Assign: Parser[(String, AST) => AST] =
    token("""=""".r).map(_ => AST.Assign(_, _))

  // Parsers that help us build an expression parser
  // args <- (expression (COMMA expression)*)
  lazy val arguments: Parser[Vector[AST]] =
    val result = for
      arg <- expression
      args <- zeroOrMore(Comma.and(expression))
    yield Vector(arg).appendedAll(args)

    result | constant(Vector())

  // call <- ID LeftParen arguments RightParen
  lazy val call: Parser[AST] = for
    callee <- Identifier
    _ <- LeftParen
    args <- arguments
    _ <- RightParen
  yield AST.Call(callee, args)

  // atom <- call / ID / Number / LeftParen expression RightParen
  lazy val atom: Parser[AST] =
    call | id | Number | LeftParen
      .and(expression)
      .flatMap(e => RightParen.and(constant(e)))

  // unary <- NOT? atom
  val unary: Parser[AST] = for
    not <- maybe(Not)
    term <- atom
  yield not match
    case Some(_) => AST.Not(term)
    case None    => term

  // A combinator to parse infix grammars
  def chainl1[A, B >: A](
      termParser: Parser[A],
      operatorParser: => Parser[(B, A) => B]
  ): Parser[B] =
    val parserOperators = zeroOrMore(operatorParser product termParser)
    (termParser, parserOperators).mapN { case (term, operator) =>
      operator.foldLeft[B](term) { case (x, (op, y)) => op(x, y) }
    }

  // product <- unary ((STAR / SLASH) unary)*
  val product: Parser[AST] = chainl1(unary, Star.|(Slash))

  // sum <- product ((PLUS / MINUS) product)*
  val sum: Parser[AST] = chainl1(product, Plus.|(Minus))

  // comparison <- sum ((EQUAL / NOT_EQUAL) sum)*
  val comparison: Parser[AST] = chainl1(sum, Equal.|(NotEqual))

  // A parser for a single expression
  lazy val expression: Parser[AST] = comparison

  // returnStatement <- RETURN expression SEMICOLON
  lazy val returnStatement: Parser[AST] =
    Return.and(expression).flatMap { term =>
      Semicolon.and(constant(AST.Return(term)))
    }

  // An expressionStatement is an expression delimited by a semicolon
  // expressionStatement <- expression SEMICOLON
  val expressionStatement: Parser[AST] =
    expression.flatMap(term => Semicolon.and(constant(term)))

  // ifStatement produces a If node
  // ifStatement <- IF LEFT_PAREN expression RIGHT_PAREN statement ELSE statement
  lazy val ifStatement: Parser[AST] = for
    conditional <- If.and(LeftParen).and(expression)
    consequence <- RightParen.and(statement)
    alternative <- Else.and(statement)
  yield AST.If(conditional, consequence, alternative)

  // blockStatement <- LEFT_BRACE statement* RIGHT_BRACE
  lazy val blockStatement: Parser[AST] = for
    statements <- LeftBrace.and(zeroOrMore(statement))
    _ <- RightBrace
  yield AST.Block(statements)

  // whileStatement <- WHILE LEFT_PAREN expression RIGHT_PAREN statement
  lazy val whileStatement: Parser[AST] = for
    conditional <- While.and(LeftParen).and(expression)
    body <- RightParen.and(statement)
  yield AST.While(conditional, body)

  // varStatement <- VAR ID ASSIGN expression SEMICOLON
  lazy val varStatement: Parser[AST] = for
    name <- Var.and(Identifier)
    value <- Assign.and(expression)
    _ <- Semicolon
  yield AST.Var(name, value)

  // assignmentStatement <- ID ASSIGN expression SEMICOLON
  val assignStatement: Parser[AST] = for
    name <- Var.and(Identifier)
    value <- Assign.and(expression)
    ast <- Semicolon.and(constant(AST.Assign(name, value)))
  yield ast

  // parameters <- (ID (COMMA ID)*)?
  val parameters: Parser[Vector[String]] = for
    param <- Identifier
    params <- zeroOrMore(Comma.and(Identifier))
    ast <- constant(Vector(param).appendedAll(params)) | (constant(Vector()))
  yield ast

  // functionStatement <- FUNCTION ID LEFT_PAREN parameters RIGHT_PAREN blockstatement
  lazy val functionStatement: Parser[AST] = for
    name <- Function.and(Identifier)
    params <- LeftParen.and(parameters)
    block <- RightParen.and(blockStatement)
  yield AST.Function(name, params, block)

  // Our statement parser
  lazy val statement: Parser[AST] =
    returnStatement
    // .or(blockStatement)
      | ifStatement
      | whileStatement
      | functionStatement
      | varStatement
      | assignStatement
      | expressionStatement

  // because our parser needs to accept more than one statement
  val parser: Parser[AST] =
    ignored.and(zeroOrMore(statement)).map(statements => AST.Block(statements))

// @main def tests =
//   val source = Source("hello1 bye2", 0)
//   val result = Parser.regex("""hello[0-9]""".r).parse(source)
//   val value = result.getValue
//   assert(value == Some("hello1"))

//   val letterOrDigit =
//     (Parser.regex("""[a-z]""".r) | (Parser.regex("""[0-9]""".r)))
//   val someLettersOrDigits = Parser.zeroOrMore(letterOrDigit)
//   val sourceB = Source("a12b34", 0)

//   val pair = for
//     first <- Parser.regex("""[0-9]+""".r)
//     second <- Parser.regex(""",""".r).and(Parser.regex("""[0-9]+""".r))
//   yield Vector(first, second)

//   val sourceC = Source("12,34", 0)

  // val stringD = """
  // function factorial(n) {
  //   var result = 1;
  //   return result;
  // }
  // if (x < 3) {
  //   print("foo")    
  // }
  // """

