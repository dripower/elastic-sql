package com.dripower.elasticsql

import org.scalatest._
import scala.util._
import org.parboiled2.ParseError

class ParserSpec extends FlatSpec with Matchers {

  def parse(input: String) = {
    val p = new Parboiled2Parser(input)
    val r = p.Query.run()
    r match {
      case Failure(e: ParseError) => println(p.formatError(e))
        case Failure(e: Throwable) => e.printStackTrace
      case _ =>
    }
    r
  }

  "Parser" should "parse simple query" in {
    val r = parse("SELECT * FROM foo")
    val expected = ast.Query(
      select = Seq(ast.SelectAll),
      from = Seq(ast.TableSource("foo", None)),
      where = Nil
    )
    r shouldEqual Success(expected)
  }
}
