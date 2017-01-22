package structures.core

/**
  * Represents a sequence.
  * @tparam A type of elements in sequence
  */
trait Seq[A] { self: Collection[A] =>
  def apply(index: Int): A
}

/**
  * Represents a sequence which has fast element access.
  * This means that [[Seq.apply]] method has O(1) time complexity.
  * @tparam A type of elements in sequence
  */
trait FastSeq[A] extends Seq[A] { self: Collection[A] =>

}
