package filters


case class Row(
  strings: Map[String, String],
  doubles: Map[String, Double],
  longs: Map[String, Long]
)

// Attributes

trait Value[R, T] extends (R => Option[T])

case class StringAttribute(name: String) extends Value[Row, String] {
  def apply(row: Row): Option[String] = row.strings.get(name)
}

case class DoubleAttribute(name: String) extends Value[Row, Double] {
  def apply(row: Row): Option[Double] = row.doubles.get(name)
}

case class LongAttribute(name: String) extends Value[Row, Long] {
  def apply(row: Row): Option[Long] = row.longs.get(name)
}

// Comparisons

trait Predicate[R] extends (R => Boolean)

case class Equals[R, T](left: Value[R, T], right: T) extends Predicate[R] {
  override def apply(row: R): Boolean = left(row).exists(_.equals(right))
}

case class In[R, T](left: Value[R, T], right: Set[T]) extends Predicate[R] {
  override def apply(row: R): Boolean = left(row).exists(right.contains)
}

case class GreaterThan[R, T](left: Value[R, T], right: T)(implicit num: Numeric[T]) extends Predicate[R] {
  override def apply(row: R): Boolean = left(row).exists(num.gt(_, right))
}

case class LessThan[R, T](left: Value[R, T], right: T)(implicit num: Numeric[T]) extends Predicate[R] {
  override def apply(row: R): Boolean = left(row).exists(num.lt(_, right))
}

// Logical operators

object And {
  def apply[R](preds: Predicate[R]*): And[R] = new And(preds)
}

case class And[R](preds: Iterable[Predicate[R]]) extends Predicate[R] {
  override def apply(row: R): Boolean = preds.forall(_.apply(row))
}

object Or {
  def apply[R](preds: Predicate[R]*): Or[R] = new Or(preds)
}

case class Or[R](preds: Iterable[Predicate[R]]) extends Predicate[R] {
  override def apply(row: R): Boolean = preds.exists(_.apply(row))
}

case class Not[R](right: Predicate[R]) extends Predicate[R] {
  override def apply(row: R): Boolean = !right(row)
}
