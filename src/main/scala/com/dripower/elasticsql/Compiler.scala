package com.dripower.elasticsql

import ast._
import io.circe._


trait Compiler[T] {
  def compile(q: Ast): (String, T)
}


class CirceCompiler extends Compiler[Json]{

  trait Show[A] {
    def show(a: A): Json
  }

  implicit class ShowSyntax[A <: Ast](val a: A){
    def show(implicit e: Show[A]) = e.show(a)
  }

  def show[A](pf: PartialFunction[A, Json]): Show[A] = new Show[A] {
    def show(ast: A) = pf(ast)
  }

  implicit def sourceShow: Show[Source] = show[Source] {
    case TableSource(table, alias) => Json.fromString(table)
  }

  implicit def literalShow: Show[Literal] = show[Literal] {
    case StringLiteral(v) => Json.fromString(v)
    case NumericLiteral(v) => Json.fromLong(v)
    case BooleanLiteral(v) => Json.fromBoolean(v)
  }

  implicit def propShow: Show[Prop] = show[Prop] {
    case Prop(obj, name: String) => Json.fromString(obj.map(_ + ".").getOrElse("") + name)
  }

  implicit def selectShow: Show[Select] = show[Select] {
    case SelectAll => Json.fromString("*")
    case SelectField(prop: Prop) => prop.show
  }

  def unquote(str: String) = {
    if(str.startsWith("\"") && str.endsWith("\"")) {
      str.tail.init
    } else str
  }

  implicit def queryShow: Show[Query] = {

    def showTermOps(ops: Seq[Op])(implicit astShow: Show[Ast]): Json = {
      val terms = ops.collect {
        case BinOp("=", left, right) => (unquote(left.show.noSpaces), right.show)
      }
      Json.obj(terms: _*)
    }

    show[Query] {
      case Query(fields, sources, where) =>
        Json.obj(
          "query" -> Json.obj(
            "term" -> showTermOps(where)
          ),
          "_source" -> Json.arr(fields.map(_.show): _*)
        )
    }
  }

  implicit def astShow: Show[Ast] = show[Ast]{
    case q: Query => q.show
    case p: Prop => p.show
    case p: Select => p.show
    case a: Literal => a.show
  }

  def indices(q: Query) = {
    q.from.collect {
      case TableSource(t, _) => t
    }.mkString(",")
  }

  def compile(a: Ast) = a match {
    case q: Query =>
      indices(q) -> q.show
  }

}
