package structures.core

/**
  * Represents a abstract collection.
  * @tparam A type of elements in collection
  */
trait Collection[A] {
  /**
    * Used to get count of elements in collection
    * @return size of collection
    */
  def size: Int

  /**
    * Used to determine collection is empty or not
    * @return true if collection is empty, false otherwise
    */
  def isEmpty: Boolean = size == 0
  /**
    * Used to determine collection is empty or not
    * @return true if collection is not empty, false otherwise
    */
  def nonEmpty: Boolean = !isEmpty

  /**
    * Used to transform elements of collection with function 'f'
    * @param f transformation function
    * @tparam B resulting elements type
    * @return new collection with transformed elements
    */
  def map[B](f: A => B): Collection[B]

  /**
    * Used to perform action on every element of collection
    * @param f function that perform action
    * @tparam U type that function returns
    * @return this collection
    */
  def foreach[U](f: A => U): Collection[A]

  /**
    * Used to get elements that satisfy predicate
    * @param f predicate function
    * @return new collection with only elements that satisfies predicates
    */
  def filter(f: A => Boolean): Collection[A]

  /**
    * Used to get elements that not satisfy predicate
    * @param f predicate function
    * @return new collection with only elements that not satisfies predicates
    */
  def filterNot(f: A => Boolean): Collection[A] = filter(f andThen {x => !x})

  /**
    * Used to aggregate elements of collection from left to right
    * @param result starting value
    * @param f function that will be calling on each element
    * @tparam B type of starting and returning value
    * @return result after passing each element
    */
  def foldLeft[B](result: B)(f: (B, A) => B): B

  /**
    * Used to aggregate elements of collection from right to left
    * @param result starting value
    * @param f function that will be calling on each element
    * @tparam B type of starting and returning value
    * @return result after passing each element
    */
  def foldRight[B](result: B)(f: (B, A) => B): B

  /**
    * Used to get first element of collection safely
    * @return Some if there is first element in collection, otherwise None
    */
  def headOption: Option[A]

  /**
    * Used to get first element of collection unsafely
    * @return first element if it exists
    * @throws UnsupportedOperationException if collection is empty
    */
  def head: A = if (nonEmpty) headOption.get else throw new UnsupportedOperationException("Call 'head' on empty collection")

  /**
    * Used to get elements of collection starting from second
    * @return new Collection with elements of this collection starting from second
    */
  def tail: Collection[A]

  /**
    * Used to get minimal element of collection
    * @param ord object that can compare elements
    * @return minimal element of collection
    */
  def min(implicit ord: Ordering[A]): A = {
    import ord._
    if (isEmpty) throw new UnsupportedOperationException("Call 'min' on empty collection")
    foldLeft(head) {
      case (a, b) if b < a => b
      case (a, _) => a
    }
  }

  /**
    * Used to get maximum element of collection
    * @param ord object that can compare elements
    * @return maximum element of collection
    */
  def max(implicit ord: Ordering[A]): A = {
    import ord._
    if (isEmpty) throw new UnsupportedOperationException("Call 'max' on empty collection")
    foldLeft(head) {
      case (a, b) if b > a => b
      case (a, _) => a
    }
  }
}
