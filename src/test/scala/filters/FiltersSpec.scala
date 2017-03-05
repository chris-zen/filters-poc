package filters

import org.scalatest.{FlatSpec, Matchers}

class FiltersSpec extends FlatSpec with Matchers {

  import FiltersSpec._

  // Attributes
  
  "An string attribute" should "extract its value from a row" in {
    StringAttribute("a")(Row1) shouldBe Some("A")
  }

  it should "extract None if it is not defined for the row" in {
    StringAttribute("c")(Row1) shouldBe None
  }

  "A double attribute" should "extract its value from a row" in {
    DoubleAttribute("c")(Row1) shouldBe Some(1.0)
  }

  it should "extract None if it is not defined for the row" in {
    DoubleAttribute("e")(Row1) shouldBe None
  }

  "A long attribute" should "extract its value from a row" in {
    LongAttribute("e")(Row1) shouldBe Some(100)
  }

  it should "extract None if it is not defined for the row" in {
    LongAttribute("a")(Row1) shouldBe None
  }

  // Comparisons
  
  "The Equals comparison" should "return true if an attribute value equals the literal" in {
    Equals(StringAttribute("a"), "A")(Row1) shouldBe true
  }

  it should "return false if an attribute value does not equal the literal" in {
    Equals(StringAttribute("a"), "B")(Row1) shouldBe false
  }

  it should "return false if an attribute value is not defined in the row" in {
    Equals(StringAttribute("c"), "C")(Row1) shouldBe false
  }

  "The In comparison" should "return true if an attribute value exists in a set of literals" in {
    In(StringAttribute("a"), Set("A", "B"))(Row1) shouldBe true
  }

  it should "return false if an attribute value does not exist in a set of literals" in {
    In(StringAttribute("a"), Set("C", "B"))(Row1) shouldBe false
  }

  it should "return false if an attribute value is not defined in the row" in {
    In(StringAttribute("c"), Set("A", "B", "C"))(Row1) shouldBe false
  }

  "The GreaterThan comparison" should "return true if an attribute value is greater than a literal" in {
    GreaterThan(DoubleAttribute("c"), 0.5).apply(Row1) shouldBe true
  }

  it should "return false if an attribute value is not greater than a literal" in {
    GreaterThan(DoubleAttribute("c"), 2.0).apply(Row1) shouldBe false
  }

  it should "return false if an attribute value is not defined in the row" in {
    GreaterThan(DoubleAttribute("e"), 0.0).apply(Row1) shouldBe false
  }

  "The LessThan comparison" should "return true if an attribute value is less than a literal" in {
    LessThan(DoubleAttribute("c"), 2.0).apply(Row1) shouldBe true
  }

  it should "return false if an attribute value is not less than a literal" in {
    LessThan(DoubleAttribute("c"), 0.5).apply(Row1) shouldBe false
  }

  it should "return false if an attribute value is not defined in the row" in {
    LessThan(DoubleAttribute("e"), 10.0).apply(Row1) shouldBe false
  }
  
  // Logical operations
  
  "The And operator" should "satisfy if both left and right predicates satisfy" in {
    And(
      Equals(StringAttribute("a"), "A"),
      GreaterThan(DoubleAttribute("c"), 0.5)
    )(Row1) shouldBe true
  }

  it should "not satisfy if any of left or right predicates doesn't satisfy" in {
    And(
      Equals(StringAttribute("a"), "B"),
      GreaterThan(DoubleAttribute("c"), 0.5)
    )(Row1) shouldBe false

    And(
      Equals(StringAttribute("a"), "A"),
      GreaterThan(DoubleAttribute("c"), 2.0)
    )(Row1) shouldBe false
  }

  "The Or operator" should "satisfy if either of left or right predicates satisfy" in {
    Or(
      Equals(StringAttribute("a"), "B"),
      GreaterThan(LongAttribute("e"), 10L)
    )(Row1) shouldBe true

    Or(
      Equals(StringAttribute("a"), "A"),
      GreaterThan(LongAttribute("e"), 1000L)
    )(Row1) shouldBe true
  }

  it should "not satisfy if both left or right predicates doesn't satisfy" in {
    Or(
      Equals(StringAttribute("a"), "B"),
      GreaterThan(LongAttribute("e"), 1000L)
    )(Row1) shouldBe false
  }

  "The Not operator" should "satisfy if its right predicate doesn't satisfy" in {
    Not(
      In(StringAttribute("b"), Set("A", "C"))
    )(Row1) shouldBe true
  }

  it should "not satisfy if its right predicate satisfies" in {
    Not(
      In(StringAttribute("b"), Set("A", "B"))
    )(Row1) shouldBe false
  }
}

object FiltersSpec {
  private val Row1 = Row(
    strings = Map(
      "a" -> "A",
      "b" -> "B"
    ),
    doubles = Map(
      "c" -> 1.0,
      "d" -> 5.0
    ),
    longs = Map(
      "e" -> 100,
      "f" -> 500
    )
  )
}