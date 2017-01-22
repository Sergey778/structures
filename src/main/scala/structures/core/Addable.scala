package structures.core

/**
  * Represents a collection that can add elements
  * @tparam A type of elements in collection
  */
trait Addable[A] { self: Collection[A] =>
  def +(that: A): Addable[A]
}

/**
  * Represents a collection that can add elements to its end
  * @tparam A type of elements in collection
  */
trait BackAddable[A] extends Addable[A] { self: Collection[A] =>
  def :+(that: A): BackAddable[A]
}

/**
  * Represents a collection that can add elements to its front
  * @tparam A type of elements in collection
  */
trait FrontAddable[A] extends Addable[A] { self: Collection[A] =>
  def +:(that: A): FrontAddable[A]
}

/**
  * Represents a collection that can add elements to its end fast.
  * This means that [[BackAddable.:+]], [[Addable.+]] has O(1) time complexity
  * @tparam A type of elements in collection
  */
trait FastBackAddable[A] extends BackAddable[A] { self: Collection[A] =>
  override def +(that: A): BackAddable[A] = this :+ that
}
/**
  * Represents a collection that can add elements to its end fast.
  * This means that [[FrontAddable.+:]], [[Addable.+]] has O(1) time complexity
  * @tparam A type of elements in collection
  */
trait FastFrontAddable[A] extends FrontAddable[A] { self: Collection[A] =>
  override def +(that: A): FrontAddable[A] = that +: this
}
