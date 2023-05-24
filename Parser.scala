import scala.util.matching.Regex

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

trait Parser[A]:
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
    def loop(source: Source, acc: Vector[A]): ParseResult[Vector[A]] =
      val result = parser.parse(source)
      result match
        case ParseResult.Success(value, newSource) =>
          loop(newSource, acc.appended(value))
        case ParseResult.Failed =>
          ParseResult.Success(acc, source)

    Parser.fromFunction(source => loop(source, Vector[A]()))

  def maybe[A](parser: Parser[A]): Parser[A] =
    parser.or(Parser.fromFunction(source => ParseResult.Failed))

  extension [A](p: Parser[A])
    /* Select between two or more alternative parser choices
     *
     * @param pb
     *   the other parser that will be tried
     * @return A parser that will try out `p` or `pb` if `p` fails
     */
    def or(pb: Parser[A]): Parser[A] =
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

@main def tests =
  val source = Source("hello1 bye2", 0)
  val result = Parser.regex("""hello[0-9]""".r).parse(source)
  val value = result.getValue
  assert(value == Some("hello1"))

  val letterOrDigit =
    (Parser.regex("""[a-z]""".r).or(Parser.regex("""[0-9]""".r)))
  val someLettersOrDigits = Parser.zeroOrMore(letterOrDigit)
  val sourceB = Source("a12b34", 0)
  println(someLettersOrDigits.parse(sourceB))

  val pair = for
    first <- Parser.regex("""[0-9]+""".r)
    second <- Parser.regex(""",""".r).and(Parser.regex("""[0-9]+""".r))
  yield Vector(first, second)

  val sourceC = Source("12,34", 0)
  println(pair.parse(sourceC))
