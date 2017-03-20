package com.dripower.elasticsql

import ast._
import org.scalatest._

class CompilerSpec extends FlatSpec with Matchers {

  val C = new CirceCompiler

  "Compiler" should "compile query" in {
    val q = Query(
      select = Seq(SelectAll),
      from = Seq(TableSource("foo", None)),
      where = Seq(BinOp("=", Prop(None, "name"), StringLiteral("bar")))
    )
    val (index, query) = C.compile(q)
    index shouldEqual "foo"
    query.noSpaces shouldEqual("""{"query":{"term":{"name":"bar"}},"_source":["*"]}""")
  }
}
