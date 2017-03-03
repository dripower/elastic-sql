package com.dripower.elasticsql

import org.parboiled2._
import org.parboiled2.{Parser => PPaarser}
import scala.util._

trait Parser {
  def parse(sql: String): Try[Ast]
}

class Parboiled2Parser(val input: ParserInput) extends PPaarser{

  def Chars = rule { oneOrMore(CharPredicate.AlphaNum ++ '_') }
  def Select = rule { ignoreCase("SELECT") }
  def From = rule { ignoreCase("FROM") }
  def Where = rule { ignoreCase("WHERE")}
  def Op = rule {

  }

  def Value = rule {
  }

  def Expr = rule {
    Ident | (Ident ~ "." ~ Ident) ~>
  }

  def Alias: Rule1[String] = rule {
    optional("as") ~ Ident
  }

  def Source = rule {
    Ident ~ optional(Alias) ~> ((a, b) => TableSource(a, b))
  }

  def Ident = rule { capture(Chars) | "`" ~ capture(CharPredicate.All) ~ "`"  }
  def query = rule {
    Select ~ From ~ Source ~ Where
  }

}
