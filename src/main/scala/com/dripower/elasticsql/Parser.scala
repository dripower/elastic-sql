package com.dripower.elasticsql

import org.parboiled2._
import org.parboiled2.{Parser => PPaarser}
import scala.util._

class Parser {
  def parse(sql: String): Try[ast.Ast] = new Parboiled2Parser(sql).Query.run()
}

class Parboiled2Parser(val input: ParserInput) extends PPaarser {


  //Keywords
  def Select = rule { ignoreCase("select") }
  def From = rule { ignoreCase("from") }
  def Where = rule { ignoreCase("where")}

  def Fields = rule {
   "*" ~ push(Seq(ast.SelectAll)) | oneOrMore(Prop).separatedBy(Sep) ~> (_.map(ast.SelectField(_)))
  }


  //Base
  def WS = rule(oneOrMore(' '))
  def OptWs = rule(zeroOrMore(' '))
  def Sep = rule(',' ~ OptWs)
  def Chars = rule { oneOrMore(CharPredicate.AlphaNum ++ '_') }
  def Digits = rule { oneOrMore(CharPredicate.Digit) }
  def Ident = rule { capture(Chars) | "`" ~ capture(noneOf("`")) ~ "`"  }



  def Literal = rule {
    capture(Digits) ~> {n => ast.NumericLiteral(n.toInt)} |
    "'" ~ capture(zeroOrMore("\\\"") | noneOf("\"")) ~ "'" ~> (ast.StringLiteral(_)) |
    ("true" ~ push(true) | "false" ~ push(false)) ~> (ast.BooleanLiteral(_))
  }

  def UnaryOp = rule {
    UnaryOperators ~ OptWs ~ Operatee ~> (ast.UnaryOp(_, _))
  }

  def BinOp = rule {
    Operatee ~ BinOperators ~ Operatee ~> {(left, op, right) => ast.BinOp(op, left, right)}
  }

  def Op = rule {
    Prop | BinOp | UnaryOp
  }

  def BinOperators = rule(capture("+"|"-"|"*"|"/"|"="|"<>"|">"|"<"|">="|"<="))
  def UnaryOperators = rule(capture("-"))
  def Operatee = rule(Literal|Prop)

  def Prop = rule {
    Ident ~> (ast.Prop(None, _)) | Ident ~ "." ~ Ident ~> {(a, b) => ast.Prop(Some(a), b)}
  }

  def Alias: Rule1[String] = rule {
    optional("as") ~ Ident
  }

  def Source: Rule1[ast.Source] = rule {
    Ident ~ optional(Alias) ~> ((a, b) => ast.TableSource(a, b))
  }

  def Sources = rule {
    oneOrMore(Source).separatedBy(Sep)
  }

  def Conds = rule {
    zeroOrMore(Op).separatedBy(Sep)
  }

  def Query = rule {
    Select ~ WS ~ Fields ~ WS ~ From ~ WS ~ oneOrMore(Source).separatedBy(Sep) ~ optional(WS ~ Where ~ WS ~ Conds) ~> {
      (fields ,sources, where) =>
      ast.Query(fields, sources, where.getOrElse(Nil))
    }
  }

}
