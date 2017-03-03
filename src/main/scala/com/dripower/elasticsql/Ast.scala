package com.dripower.elasticsql

sealed trait Ast
sealed trait Query extends Ast
sealed trait Source extends Ast

case class TableSource(table: String, alias: Option[String])

case class FlattenQuery(
  from: List[Source],
  where: Option[Ast],
  limit: Option[Ast],
  offset: Option[Ast]
)
