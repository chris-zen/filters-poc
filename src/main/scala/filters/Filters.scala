package filters


case class Row(
  strings: Map[String, String],
  doubles: Map[String, Double],
  longs: Map[String, Long]
)

// Attributes

trait Value[T] extends (Row => Option[T])

case class StringAttribute(name: String) extends Value[String] {
  def apply(row: Row): Option[String] = row.strings.get(name)
}

case class DoubleAttribute(name: String) extends Value[Double] {
  def apply(row: Row): Option[Double] = row.doubles.get(name)
}

case class LongAttribute(name: String) extends Value[Long] {
  def apply(row: Row): Option[Long] = row.longs.get(name)
}

// Comparisons

trait Predicate extends (Row => Boolean)

case class Equals[T](left: Value[T], right: T) extends Predicate {
  override def apply(row: Row): Boolean = left(row).exists(_.equals(right))
}

case class In[T](left: Value[T], right: Set[T]) extends Predicate {
  override def apply(row: Row): Boolean = left(row).exists(right.contains)
}

case class GreaterThan[T](left: Value[T], right: T)(implicit num: Numeric[T]) extends Predicate {
  override def apply(row: Row): Boolean = left(row).exists(num.gt(_, right))
}

case class LessThan[T](left: Value[T], right: T)(implicit num: Numeric[T]) extends Predicate {
  override def apply(row: Row): Boolean = left(row).exists(num.lt(_, right))
}

// Logical operators

object And {
  def apply(preds: Predicate*): And = new And(preds)
}

case class And(preds: Iterable[Predicate]) extends Predicate {
  override def apply(row: Row): Boolean = preds.forall(_.apply(row))
}

object Or {
  def apply(preds: Predicate*): Or = new Or(preds)
}

case class Or(preds: Iterable[Predicate]) extends Predicate {
  override def apply(row: Row): Boolean = preds.exists(_.apply(row))
}

case class Not(right: Predicate) extends Predicate {
  override def apply(row: Row): Boolean = !right(row)
}
