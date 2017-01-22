package structures.mutable

import structures.core.{Collection, FastSeq, Seq}

/**
  * Represents a sequence whose elements can be update
  * @tparam A type of elements in sequence
  */
trait MutableSeq[A] extends Seq[A] { self: Collection[A] =>
  def update(index: Int, newValue: A): MutableSeq[A]
}

/**
  * Represents a sequence whose elements can be update fast.
  * This means that [[MutableSeq.update]], [[Seq.apply]] methods complexity is O(1)
  * @tparam A type of elements in sequence
  */
trait FastMutableSeq[A] extends FastSeq[A] with MutableSeq[A] { self: Collection[A] =>

}
