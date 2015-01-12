package x7c1.parse.lib

import org.scalatest.{FunSpec, Matchers}

class ParseLibraryTest extends FunSpec with Matchers {

  describe(ParseLibrary.getClass.getSimpleName){
    it("should create message"){
      val message = ParseLibrary createMessageFor "earthlings"
      message should be("hello, earthlings!")
    }
  }
}

class ParseNotifierClientTest extends FunSpec with Matchers {

  object column extends ColumnDefinitions with DefaultAdapters{
    val channels = define[String]("channels")
    val userName = define[String]("userName")
    val userId = define[Int]("userId")
    val score = define[Int]("score")
  }

  describe(ParseNotifierClient.getClass.getSimpleName) {
    it("should express where(in)") {
      val query = ParseQuery().where(
        column.channels is "",
        column.userName in("foo", "bar")
      )
      query.jsonBody shouldBe
        """{
          |  "where": {
          |    "channels": "",
          |    "userName": {
          |      "$in": ["foo", "bar"]
          |    }
          |  }
          |}""".stripMargin
    }
    it("should express where(> and <=) by same key") {
      val query = ParseQuery().where(
        (column.userId > 100) and (column.userId <= 1000)
      )
      query.jsonBody shouldBe
        """{
          |  "where": {
          |    "userId": {
          |      "$gt": 100,
          |      "$lte": 1000
          |    }
          |  }
          |}""".stripMargin
    }
    it("should express where(> and <=) by different keys") {
      val query = ParseQuery().where(
        (column.userId > 123) and (column.score > 345)
      )
      query.jsonBody shouldBe
        """{
          |  "where": {
          |    "userId": {
          |      "$gt": 123
          |    },
          |    "score": {
          |      "$gt": 345
          |    }
          |  }
          |}""".stripMargin
    }
    it("should express where(>= and <)") {
      val query = ParseQuery().where(
        (column.userId >= 100) and (column.userId < 1000)
      )
      query.jsonBody shouldBe
        """{
          |  "where": {
          |    "userId": {
          |      "$gte": 100,
          |      "$lt": 1000
          |    }
          |  }
          |}""".stripMargin
    }
    it("should express sequential where") {
      val query = ParseQuery().
        where(column.userId >= 100).
        where(column.userName in ("foo", "bar"))

      query.jsonBody shouldBe
        """{
          |  "where": {
          |    "userId": {
          |      "$gte": 100
          |    },
          |    "userName": {
          |      "$in": ["foo", "bar"]
          |    }
          |  }
          |}""".stripMargin
    }

  }

}
