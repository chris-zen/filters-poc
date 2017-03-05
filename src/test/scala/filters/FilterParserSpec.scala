package filters

import org.scalatest.{FlatSpec, Matchers}

class FilterParserSpec extends FlatSpec with Matchers {

  "A filter" should "be parsed from JSON" in {
    val json =
      """{
        |  "and": [
        |    {
        |      "eq": {
        |        "attr": { "string": "a" },
        |        "literal": "A"
        |      }
        |    },
        |    {
        |      "not": {
        |        "in": {
        |          "attr": { "string": "b" },
        |          "values": ["A", "C"]
        |        }
        |      }
        |    },
        |    {
        |      "gt": {
        |        "attr": { "long": "e" },
        |        "literal": 10
        |      }
        |    }
        |  ]
        |}
      """.stripMargin

    FilterParser.from(json) shouldBe And(
      Equals(StringAttribute("a"), "A"),
      Not(In(StringAttribute("b"), Set("A", "B"))),
      GreaterThan(LongAttribute("e"), 10L)
    )
  }
}
