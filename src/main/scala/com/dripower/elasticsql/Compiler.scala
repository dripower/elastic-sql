package com.dripower.elasticsql

import ast._
import io.circe._


trait Compiler[T] {
  def compile(q: Ast): T
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

  implicit lazy val queryShow = show[Query] {
    case Query(fields, sources, where) =>
      Json.obj()
  }

  implicit lazy val astShower = show[Ast]{
    case q: Query => q.show
  }

  def compile(a: Ast) = a.show

}
