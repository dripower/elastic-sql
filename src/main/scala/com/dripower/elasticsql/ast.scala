package com.dripower.elasticsql

package object ast {
  sealed trait Ast

  sealed trait Source extends Ast
  case class TableSource(table: String, alias: Option[String]) extends Source

  sealed trait Select extends Ast
  case object SelectAll extends Select
  case class SelectField(prop: Prop) extends Select

  sealed trait Literal extends Ast
  case class StringLiteral(value: String) extends Literal
  case class NumericLiteral(value: Long) extends Literal
  case class BooleanLiteral(value: Boolean) extends Literal

  case class Query(
    select: Seq[Select],
    from: Seq[Source],
    where: Seq[Op]
  ) extends Ast

  sealed trait Op extends Ast
  case class BinOp(op: String, left: Ast, right: Ast) extends Op
  case class UnaryOp(op: String, expr: Ast) extends Op
  case class Prop(obj: Option[String], name: String) extends Op
}
