package x7c1.lilac.lib

import org.specs2.mutable.Specification
import x7c1.lilac.lib.Modifiers.{Repeated, Optional, Required}

object LilacLibraryTest extends Specification {

  LilacLibrary.getClass.getSimpleName should {
    "create message" in {
      val message = LilacLibrary createMessageFor "earthlings"
      "hello, earthlings!" === message
    }
    "parse required field" in {
      val text = "required string user_name = 11;"
      val field = Parser.parseAll(Parser.field, text).get
      field.fieldRule === Required
      field.fieldType === "string"
      field.fieldName === "user_name"
      field.tagNumber === 11
    }
    "parse optional field" in {
      val text = "optional int user_id = 22;"
      val field = Parser.parseAll(Parser.field, text).get
      field.fieldRule === Optional
    }
    "parse repeated field" in {
      val text = "repeated string emails = 33;"
      val field = Parser.parseAll(Parser.field, text).get
      field.fieldRule === Repeated
    }
    "parse message" in {
      val text = "message SampleUser { required string user_name = 1; }"
      val message = Parser.parseAll(Parser.message, text).get
      message.name === "SampleUser"

      val ((field: Field) :: tail) = message.body.nodes
      field.fieldName === "user_name"
      field.fieldRule === Required
      field.fieldType === "string"
    }
    "parse nested node in message" in {
      val text =
        """message SampleUser {
          |  required int32 user_id = 1;
          |  message NestedUser { optional string user_name = 123; }
          |}""".stripMargin

      val message = Parser.parseAll(Parser.message, text).get
      message.name === "SampleUser"

      val (field: Field) :: (nestedMessage: Message) :: _ = message.body.nodes
      field.fieldName === "user_id"
      field.fieldType === "int32"

      nestedMessage.name === "NestedUser"
      val (nestedField: Field) :: _ = nestedMessage.body.nodes
      nestedField.fieldName === "user_name"
    }
    "parse enum" in {
      val text =
        """enum Alcohol{
          |  WHISKY = 1;
          |  ALE = 2;
          |  LAGER = 3;
          |}""".stripMargin

      val enum = Parser.parseAll(Parser.enum, text).get
      enum.name === "Alcohol"
      enum.constants.map(_.value) === Seq("WHISKY", "ALE", "LAGER")
      enum.constants.map(_.tagNumber) === Seq(1, 2, 3)
    }
    "parse enum in message" in {
      val text =
        """message SampleUser {
          |  enum UserKind {
          |    NORMAL = 1;
          |    ADMIN = 2;
          |  }
          |}""".stripMargin

      val message = Parser.parseAll(Parser.message, text).get
      val (enum: Enum) :: _ = message.body.nodes
      enum.name === "UserKind"
    }
  }
}
