package x7c1.noir.lib

import org.scalatest.{FunSpec, Matchers}

class NoirLibraryTest extends FunSpec with Matchers {

  describe(NoirLibrary.getClass.getSimpleName){
    it("should create message"){
      val message = NoirLibrary createMessageFor "earthlings"
      message should be("hello, earthlings!")
    }
  }

  describe(NoirParser.getClass.getSimpleName){
    it("should parse identifier"){
      val text = "foo"
      val i = NoirParser.parseAll(NoirParser.identifier, text).get
      i should be("foo")
    }

    it("should parse tree"){
      val text = "x(y,z)"
      val tree = NoirParser.parseAll(NoirParser.tree, text).get
      tree.name shouldBe "x"
      tree.nodes shouldBe Seq(Leaf("y"), Leaf("z"))
    }

    it("should parse nested tree"){
      val text = "x(y,jp.example.noir(b,c),z1.z2)"
      val tree = NoirParser.parseAll(NoirParser.tree, text).get
      tree.name shouldBe "x"
      tree.nodes shouldBe Seq(
        Leaf("y"),
        Tree("jp.example.noir", Seq(Leaf("b"), Leaf("c"))),
        Leaf("z1.z2")
      )
    }
  }
  describe(NoirParser.getClass.getSimpleName){
    it("should parse method-call"){
      val text = "x().setFoo()"
      val tree = NoirParser.parseAll(NoirParser.receiverTree, text).get
      tree.message.name shouldBe "setFoo"
      tree.tree.name shouldBe "x"
    }
  }
  describe(NoirRenderer.getClass.getSimpleName){
    it("should render tree"){
      val text = """x(y,z(123),a(b,c))"""
      val tree = NoirParser.parseAll(NoirParser.tree, text).get
      val source = NoirRenderer.prettyPrint(tree)
      source shouldBe
        """x(
          |  y,
          |  z(123),
          |  a(
          |    b,
          |    c))""".stripMargin
    }
    /*
    it("should render AST"){
      val text = """PackageDef(Select(Select(Ident(x7c1), x7c1.brown), x7c1.brown.app), List(ClassDef(Modifiers(ABSTRACT | INTERFACE | DEFAULTPARAM/TRAIT), x7c1.brown.app.Foo, List(TypeDef(Modifiers(PARAM), TypeName("A"), List(), TypeTree().setOriginal(TypeBoundsTree(TypeTree(), TypeTree()))), TypeDef(Modifiers(PARAM), TypeName("B"), List(), TypeTree().setOriginal(TypeBoundsTree(TypeTree(), TypeTree())))), Template(List(Select(Ident(scala), TypeName("AnyRef"))), noSelfType, List(DefDef(Modifiers(DEFERRED), TermName("foo"), List(), List(), TypeTree().setOriginal(AppliedTypeTree(Select(Ident(scala), scala.Tuple2), List(TypeTree().setOriginal(AppliedTypeTree(Select(Ident(scala), scala.Tuple2), List(TypeTree().setOriginal(Ident(TypeName("A"))), TypeTree().setOriginal(Ident(TypeName("B")))))), TypeTree().setOriginal(AppliedTypeTree(Select(Ident(scala), scala.Tuple2), List(TypeTree().setOriginal(Ident(TypeName("A"))), TypeTree().setOriginal(Ident(TypeName("B"))))))))), EmptyTree)))), ClassDef(Modifiers(ABSTRACT | DEFAULTPARAM/TRAIT), x7c1.brown.app.Bar, List(), Template(List(Select(Ident(scala), TypeName("AnyRef"))), noSelfType, List(DefDef(Modifiers(), TermName("$init$"), List(), List(List()), TypeTree(), Block(List(), Literal(Constant(())))), DefDef(Modifiers(), TermName("bar"), List(), List(), TypeTree(), Literal(Constant("bar")))))), ClassDef(Modifiers(ABSTRACT | INTERFACE | DEFAULTPARAM/TRAIT), x7c1.brown.app.Baz, List(TypeDef(Modifiers(PARAM), TypeName("A"), List(), TypeTree().setOriginal(TypeBoundsTree(TypeTree(), TypeTree())))), Template(List(Select(Ident(scala), TypeName("AnyRef"))), noSelfType, List(DefDef(Modifiers(DEFERRED), TermName("baz"), List(), List(), TypeTree().setOriginal(AppliedTypeTree(Select(Select(Ident(_root_), scala), scala.Function1), List(TypeTree().setOriginal(Ident(TypeName("A"))), TypeTree().setOriginal(Ident(TypeName("A")))))), EmptyTree)))), ClassDef(Modifiers(ABSTRACT | INTERFACE | DEFAULTPARAM/TRAIT), x7c1.brown.app.MergedSample, List(), Template(List(TypeTree(), TypeTree().setOriginal(AppliedTypeTree(Ident(x7c1.brown.app.Foo), List(TypeTree().setOriginal(Select(Select(This(TypeName("scala")), scala.Predef), TypeName("String"))), TypeTree().setOriginal(Select(Ident(scala), scala.Int))))), TypeTree().setOriginal(Ident(x7c1.brown.app.Bar)), TypeTree().setOriginal(AppliedTypeTree(Ident(x7c1.brown.app.Baz), List(TypeTree().setOriginal(Select(Ident(scala), scala.Long)))))), noSelfType, List()))))"""
      val tree = NoirParser.parseAll(NoirParser.node, text).get
      val source = NoirRenderer.prettyPrint(tree)
      println(source)
    }
    // */
  }
}
