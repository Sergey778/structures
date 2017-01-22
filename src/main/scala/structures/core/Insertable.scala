package structures.core

/**
  * Represents a collection that can insert elements
  * @tparam A type of elements in collection
  */
trait Insertable[A] { self: Collection[A] =>
  def insert(onIndex: Int, value: A): Insertable[A]
}

/**
  * Represents a collection that can insert elements fast.
  * This means that [[Insertable.insert]] method has O(1) time complexity
  * @tparam A type of elements in collection
  */
trait FastInsertable[A] extends Insertable[A] { self: Collection[A] =>

}
