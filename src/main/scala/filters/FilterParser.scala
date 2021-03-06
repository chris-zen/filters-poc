package filters


object FilterParser {

  def from(json: String): Predicate[Row] = {
    And(
      Equals(StringAttribute("a"), "A"),
      Not(In(StringAttribute("b"), Set("A", "B"))),
      GreaterThan(LongAttribute("e"), 10L)
    )
  }
}
