package structures.mutable

import structures.core._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
  * ArrayList is collection class that provides fast random access operations and fast append to end of collection.
  * This class is based on array that extends when needed.
  * @param _capacity Basic capacity of array. Default is taken from ArrayList.defaultCapacity
  * @tparam A Type of elements in ArrayList
  */
class ArrayList[A] private (private[this] val _capacity: Int = ArrayList.defaultCapacity)
  extends Collection[A]
    with FastMutableSeq[A]
    with FastBackAddable[A]
    with Insertable[A]
    with FrontAddable[A] { self =>

  private var array: Array[A] = resizedArray(_capacity)

  private var length: Int = 0

  /**
    * This method is used to resize array or return error of resizing.
    * If capacity is greater than length of ArrayList - method resizes array and saves all elements
    * If capacity is less than length of ArrayList - method resizes array and saves elements up to new capacity
    * If capacity is less than zero - returns Failure with IllegalArgumentException
    * Time complexity is O(n)
    * @example
    *          val a = ArrayList(1, 2, 3, 4) // capacity = 4
    *          val s = a.tryReserve(40) // capacity = 40; s = Success
    *          val f = a.tryReserve(-10) // capacity = 40; f = Failure
    *          val b = a.tryReserve(2) // capacity = 2; b = Success
    *          println(a) // prints: ArrayList(1, 2)
    * @param capacity new capacity of ArrayList
    * @return Success object with resized ArrayList or Failure with IllegalArgumentException
    */
  def tryReserve(capacity: Int): Try[ArrayList[A]] =
    if (capacity < 0) Failure(new IllegalArgumentException("Capacity must be greater than zero"))
    else Success(reserve(capacity))

  /**
    * This method is used to resize array.
    * If capacity is greater than length of ArrayList - method resizes array and saves all elements
    * If capacity is less than length of ArrayList - method resizes array and saves elements up to new capacity
    * If capacity is less than zero - does nothing
    * Warning! On illegal argument this method does nothing. If you need explicit error returning use 'tryReserve'
    * Time complexity is O(n)
    * @example
    *          val a = ArrayList(1, 2, 3, 4) // capacity = 4
    *          val s = a.reserve(40) // capacity = 40
    *          a eq s // true
    *          val f = a.reserve(-10) // capacity = 40
    *          a eq f // true
    *          val b = a.reserve(2) // capacity = 2
    *          a eq b // true
    *          println(a) // prints: ArrayList(1, 2)
    * @param capacity new capacity of ArrayList
    * @return resized ArrayList
    */
  def reserve(capacity: Int): ArrayList[A] = {
    if (capacity >= 0) array = resizedArray(capacity)
    this
  }

  /**
    * This method is used to insert value on some index.
    * Elements that have index bigger than passing are shifting to right
    * If passing index is less than 0 or greater than last index of ArrayList - does nothing
    * On other cases - insert element
    * Warning! On illegal argument this method does nothing. If you need explicit error returning use 'tryInsert'
    * Time complexity is O(n)
    * @example
    *          val a = ArrayList(1, 2, 3, 4)
    *          a.insert(0, -1)
    *          println(a) // prints: ArrayList(-1, 1, 2, 3, 4)
    *          a.insert(1, -2)
    *          println(a) // prints: ArrayList(-1, -2, 1, 2, 3, 4)
    *          a.insert(-1, 1)
    *          println(a) // prints: ArrayList(-1, -2, 1, 2, 3, 4)
    * @param onIndex index of inserting element
    * @param value value of inserting element
    * @return this ArrayList with inserted value
    */
  override def insert(onIndex: Int, value: A): ArrayList[A] =
    if (isInBounds(onIndex)) insertWithoutCheck(onIndex, value)
    else this

  /**
    * This method is used to insert value on some index or return error of inserting.
    * Elements that have index bigger than passing are shifting to right
    * If passing index is less than 0 or greater than last index of ArrayList - returns Failure
    * On other cases - insert element
    * Warning! On illegal argument this method does nothing. If you need explicit error returning use 'tryInsert'
    * Time complexity is O(n)
    * @example
    *          val a = ArrayList(1, 2, 3, 4)
    *          a.tryInsert(0, -1) // returns Success
    *          println(a) // prints: ArrayList(-1, 1, 2, 3, 4)
    *          a.tryInsert(1, -2) // returns Success
    *          println(a) // prints: ArrayList(-1, -2, 1, 2, 3, 4)
    *          a.tryInsert(-1, 1) // returns Failure
    *          println(a) // prints: ArrayList(-1, -2, 1, 2, 3, 4)
    * @param onIndex index of inserting element
    * @param value value of inserting element
    * @return Success when successfully inserted value or Failure with IllegalArgumentException
    */
  def tryInsert(onIndex: Int, value: A): Try[ArrayList[A]] = {
    if (isInBounds(onIndex)) Success(insertWithoutCheck(onIndex, value))
    else Failure(
      new IllegalArgumentException("Index must be greater or equal to zero and lower than length of ArrayList")
    )
  }

  private def insertWithoutCheck(onIndex: Int, value: A): ArrayList[A] = {
    shiftRight(onIndex)
    array(onIndex) = value
    this
  }

  /**
    * Used to prepend element to ArrayList
    * Time complexity is O(n)
    * @example
    *          val a = ArrayList(1, 2)
    *          0 +: a
    *          println(a) // prints: ArrayList(0, 1, 2)
    * @param that element to prepend
    * @return this ArrayList with prepended element
    */
  override def +:(that: A): ArrayList[A] = {
    shiftRight(0)
    array(0) = that
    this
  }

  private def shiftRight(from: Int, count: Int = 1): ArrayList[A] = {
    if (length + count >= array.length) array = resizedArray()
    length += count
    (length - 1 until from).by(-1).foreach { i =>
      array(i) = array(i - count)
    }
    this
  }

  /**
    * Used to remove element at some index.
    * If index is not in bounds of ArrayList - does nothing
    * On other cases - removes element
    * Warning! On illegal argument this method does nothing. If you need explicit error returning use 'tryRemoveAt'
    * Time complexity is O(n)
    * @example
    *          val a = ArrayList(1, 2, 3, 4)
    *          a.removeAt(0)
    *          println(a) // prints: ArrayList(2, 3, 4)
    *          a.removeAt(1)
    *          println(a) // prints: ArrayList(2, 4)
    *          a.removeAt(-1)
    *          println(a) // prints: ArrayList(2, 4)
    * @param index index of removing element
    * @return this ArrayList without removing element
    */
  def removeAt(index: Int): ArrayList[A] = {
    if (isInBounds(index)) removeAtWithoutCheck(index)
    else this
  }

  /**
    * Used to remove element at some index with explicit error returning.
    * If index is not in bounds of ArrayList - returns Failure
    * On other cases - removes element
    * @example
    *          val a = ArrayList(1, 2, 3, 4)
    *          a.tryRemoveAt(0) // Success
    *          println(a) // prints: ArrayList(2, 3, 4)
    *          a.tryRemoveAt(1) // Success
    *          println(a) // prints: ArrayList(2, 4)
    *          a.tryRemoveAt(-1) // Failure
    *          println(a) // prints: ArrayList(2, 4)
    * @param index index of removing element
    * @return Success if element was removed or Failure with IllegalArgumentException otherwise
    */
  def tryRemoveAt(index: Int): Try[ArrayList[A]] = {
    if (isInBounds(index)) Success(removeAtWithoutCheck(index))
    else Failure(
      new IllegalArgumentException("Index must be greater or equal to zero and lower than length of ArrayList")
    )
  }

  private def removeAtWithoutCheck(index: Int): ArrayList[A] = shiftLeft(index)

  private def shiftLeft(to: Int, count: Int = 1): ArrayList[A] = {
    (to + count until length).foreach { i =>
      array(i - count) = array(i)
    }
    length -= count
    this
  }
  /**
    * This method is used to change value of element on some index.
    * If onIndex is greater than length of ArrayList or less than 0 - does nothing
    * On other cases - updates value
    * Warning! On illegal argument this method does nothing. If you need explicit error returning use 'tryUpdate'
    * Time complexity is O(1)
    * @example
    *          val a = ArrayList(1, 2, 3, 4)
    *          a.update(0, -1)
    *          println(a) // prints ArrayList(-1, 2, 3, 4)
    *          a(2) = -2 // also can be called in that way
    *          println(a) // prints ArrayList(-1, 2, -2, 4)
    *          a.update(-1, -1) // does nothing
    *          println(a) // prints ArrayList(-1, 2, -2, 4)
    * @param index index of value that need to be updated
    * @param newValue new value of element
    * @return this ArrayList with updated element
    */
  override def update(index: Int, newValue: A): ArrayList[A] =
    if (isInBounds(index)) updateWithoutCheck(index, newValue)
    else this
  /**
    * This method is used to change value of element on some index with explicit error returning.
    * If onIndex is greater than length of ArrayList or less than 0 - does nothing
    * On other cases - updates value
    * Time complexity is O(1)
    * @example
    *          val a = ArrayList(1, 2, 3, 4)
    *          a.tryUpdate(0, -1) // returns Success
    *          println(a) // prints ArrayList(-1, 2, 3, 4)
    *          a.tryUpdate(-1, -1) // returns Failure
    *          println(a) // prints ArrayList(-1, 2, -2, 4)
    * @param index index of value that need to be updated
    * @param newValue new value of element
    * @return Success if element was updated or Failure with IllegalArgumentException otherwise
    */
  def tryUpdate(index: Int, newValue: A): Try[ArrayList[A]] = {
    if (isInBounds(index)) Success(updateWithoutCheck(index, newValue))
    else Failure(
      new IllegalArgumentException("Index must be greater or equal to zero and lower than length of ArrayList")
    )
  }

  private def updateWithoutCheck(index: Int, newValue: A): ArrayList[A] = {
    array(index) = newValue
    this
  }

  /**
    * Used to return count of max elements that can be stored in ArrayList without resizing
    * @example
    *          val a = ArrayList(1, 2, 3, 4)
    *          a.capacity // 4
    *          ArrayList[Int]().capacity // defaultCapacity
    *          ArrayList.withCapacity[Int](20) // 20
    * @return capacity of array
    */
  def capacity: Int = array.length

  override def filter(f: (A) => Boolean): ArrayList[A] = {
    val list = ArrayList.withCapacity[A](length)
    for {
      i <- 0 until length
      if f(this (i))
    } list :+ this (i)
    list
  }

  override def foldLeft[B](result: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(rt: B, index: Int): B =
      if (index < length) loop(f(rt, this(index)), index + 1)
      else rt

    loop(result, 0)
  }

  override def foldRight[B](result: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(rt: B, index: Int): B =
      if (index >= 0) loop(f(rt, this(index)), index - 1)
      else rt

    loop(result, length - 1)
  }

  /**
    * Used to take n elements from start of ArrayList
    * If count <= 0 - returns empty ArrayList
    * If count >= length - returns copy of this ArrayList
    * Otherwise - returns ArrayList with n first elements from this
    * This method does not mutate ArrayList.
    * Time complexity is O(n)
    * @example
    *          val a = ArrayList(1, 2, 3, 4)
    *          a.take(2) // returns ArrayList(1, 2)
    *          a.take(-1) // returns ArrayList()
    *          a.take(10) // returns ArrayList(1, 2, 3, 4)
    * @param count count of elements that should be taken
    * @return new ArrayList with elements from start of this ArrayList
    */
  def take(count: Int): ArrayList[A] = {
    if (count >= length) map(x => x)
    else if (count <= 0) ArrayList.empty[A]
    else {
      val result = ArrayList.withCapacity[A](count)
      for (i <- 0 until count) result :+ this (i)
      result
    }
  }

  /**
    * Used to split array to two at specified index
    * If index <= 0 - returns empty ArrayList and copy of this ArrayList
    * If index >= length - return copy of this ArrayList and empty ArrayList
    * Otherwise = returns two arrays
    * Time complexity is O(n)
    * @example
    *          val a = ArrayList(1, 2, 3, 4)
    *          a.splitAt(1) // returns (ArrayList(1), ArrayList(2, 3, 4))
    *          a.splitAt(0) // returns (ArrayList(), ArrayList(1, 2, 3, 4))
    *          a.splitAt(10) // return (ArrayList(1, 2, 3, 4), ArrayList())
    * @param index index which must be border of two ArrayLists
    * @return two ArrayLists, first that contains elements before index and second that contains elements after
    */
  def splitAt(index: Int): (ArrayList[A], ArrayList[A]) = {
    if (index < 0) (ArrayList.empty[A], this.map(x => x))
    else if (index >= length) (this.map(x => x), ArrayList.empty[A])
    else (take(index), skip(index))
  }

  /**
    * Used to create iterator of ArrayList
    * Time Complexity is O(1)
    * @example
    *          val a = ArrayList(1, 2, 3, 4)
    *          val it = a.iterator
    *          it.hasNext // returns true
    *          it.next // returns 1
    *          it.next // returns 2
    *          it.next // returns 3
    *          it.next // returns 4
    *          it.hasNext // returns false
    * @return iterator of this ArrayList
    */
  def iterator: Iterator[A] = new Iterator[A] {

    private var index: Int = 0

    override def hasNext: Boolean = index < self.size

    override def next(): A = {
      index = index + 1
      apply(index - 1)
    }
  }

  /**
    * Used to getcount of elements that ArrayList contains
    * Time complexity is O(1)
    * @example
    *          val a = ArrayList(1, 2, 3, 4, 5)
    *          a.size // returns 5
    *          a :+ 10
    *          a.size // returns 6
    *          ArrayList[Int]().size // returns 0
    * @return count of elements that ArrayList contains
    */
  override def size: Int = length

  /**
    * Used to safely get first element of ArrayList
    * Time complexity is O(1)
    * @example
    *          val a = ArrayList(1, 2)
    *          a.headOption // returns Some(1)
    *          val b = ArrayList.empty[Int]
    *          b.headOption // returns None
    * @return Some if there is at least one element, None otherwise
    */
  override def headOption: Option[A] = get(0)

  /**
    * Used to safely get element at specified index
    * Time complexity is O(1)
    * @example
    *          val a = ArrayList(1, 2, 3)
    *          a.get(0) // returns Some(1)
    *          a.get(2) // returns Some(3)
    *          a.get(-1) // returns None
    *          a.get(10) // returns None
    * @param index index of element
    * @return Some if there is element with that index, None otherwise
    */
  def get(index: Int): Option[A] =
    if (isInBounds(index)) Some(apply(index))
    else None

  private def isInBounds(index: Int): Boolean = 0 <= index && index < length

  /**
    * Used to get element at specified index.
    * Warning! If index is not in bounds of ArrayList exception will be thrown.
    * If you want safe alternative - use 'get' method.
    * Time complexity is O(1)
    * @example
    *          val a = ArrayList(1, 2, 3, 4, 5)
    *          a(0) // returns 1
    *          a(3) // returns 4
    *          a(-1) // throws ArrayIndexOutOfBoundsException
    * @param index index of element
    * @return element on specified index
    * @throws ArrayIndexOutOfBoundsException if index is not in bound of ArrayList
    */
  override def apply(index: Int): A = array(index)

  /**
    * Used to return elements of ArrayList except the first one.
    * If this ArrayList is empty - returns new empty ArrayList
    * This method does not mutate this ArrayList
    * Time complexity is O(n)
    * @example
    *          val a = ArrayList(1, 2, 3)
    *          a.tail // returns ArrayList(2, 3)
    *          a.tail.tail // returns ArrayList(3)
    *          ArrayList[Int]().tail // returns ArrayList()
    * @return elements of ArrayList from index 1
    */
  def tail: ArrayList[A] = skip(1)

  /**
    * Used to skip n elements from start of ArrayList and return else
    * If count >= length - returns empty ArrayList
    * If count <= 0 - returns copy of this ArrayList
    * Otherwise - returns ArrayList with last elements from this
    * This method does not mutate this ArrayList.
    * Time complexity is O(n)
    * @example
    *          val a = ArrayList(1, 2, 3, 4)
    *          a.skip(2) // returns ArrayList(3, 4)
    *          a.skip(-1) // returns ArrayList(1, 2, 3, 4)
    *          a.skip(10) // returns ArrayList()
    * @param count count of elements that should be skipped
    * @return new ArrayList with elements from end of this ArrayList
    */
  def skip(count: Int): ArrayList[A] = {
    if (count > length) ArrayList.empty[A]
    else if (count <= 0) map(x => x)
    else {
      val result = ArrayList.withCapacity[A](length - count)
      for (i <- count until length) result :+ this (i)
      result
    }
  }

  override def map[B](f: (A) => B): ArrayList[B] = {
    val list = ArrayList.withCapacity[B](length)
    for {
      i <- 0 until length
      r = f(this (i))
    } list :+ r
    list
  }

  /**
    * Used to append element to end of ArrayList
    * If capacity of ArrayList equal to its size this operation will require resizing
    * Time complexity is amortized O(1)
    * @example
    *          val a = ArrayList(-1, 0, 1, 2, 3, 4) // capacity = 6
    *          a :+ 5 // returns ArrayList(1, 2, 3, 4, 5), will take O(n) time because capacity needs to be increased
    *          a :+ 6 // returns ArrayList(1, 2, 3, 4, 5, 6), will take O(1) time
    *          a :+ 7 // returns ArrayList(1, 2, 3, 4, 5, 6, 7), will take O(1) time
    * @param that appending element
    * @return this ArrayList with appended element
    */
  override def :+(that: A): ArrayList[A] = {
    val newLength = length + 1
    if (array.length < newLength) {
      array = resizedArray()
    }
    array(length) = that
    length = newLength
    this
  }

  private def resizedArray(newCapacity: Int = (array.length * ArrayList.growthFactor).toInt) = {
    val newArray = new Array[Any](newCapacity)
    length = Math.min(newCapacity, length)
    for (i <- 0 until length) newArray(i) = array(i)
    newArray.asInstanceOf[Array[A]]
  }

  /**
    * Used to clear all elements
    * Time complexity is O(1)
    * @example
    *          val a = ArrayList(1, 2, 3, 4)
    *          a.clear() // returns ArrayList()
    * @return this ArrayList without elements
    */
  def clear(): ArrayList[A] = {
    length = 0
    this
  }

  /**
    * Used to compare this ArrayList with other ArrayList for equality
    * ArrayLists are equal if their length are same and elements match
    * Warning! As other ArrayList can be passed anything you want. But only arrays will be compared.
    * Time complexity is O(n)
    * @example
    *          val a = ArrayList(1, 2, 3, 4)
    *          val b = ArrayList(1, 2, 3, 4)
    *          a == b // returns true
    *          a == a // returns true
    *          a == (b :+ 5) // returns false
    *          a == ArrayList.empty[Int] // returns false
    *          a == "abc" // returns false
    * @param obj object to compare
    * @return true if arrays are equal, false otherwise
    */
  override def equals(obj: scala.Any): Boolean = obj match {
    case a: ArrayList[A] if this.length == a.length =>
      !(for (i <- 0 until length) yield this(i) == a(i)).contains(false)
    case _ => false
  }

  /**
    * Used to make string representation of ArrayList
    * Time complexity is O(n)
    * @example
    *          ArrayList(1, 2, 3, 4).toString // returns "ArrayList(1, 2, 3, 4)"
    *          ArrayList(1).toString // returns "ArrayList(1)"
    *          ArrayList.empty[Int].toString // returns ArrayList()
    * @return string representation of this ArrayList
    */
  override def toString: String = {
    if (isEmpty) "ArrayList()"
    else {
      val s = new StringBuilder("ArrayList(")
      foreach(x => s.append(x.toString).append(", "))
      s.dropRight(2).append(")").mkString
    }
  }

  override def foreach[@specialized(Unit) U](f: (A) => U): ArrayList[A] = {
    for {
      i <- 0 until length
    } f(this (i))
    this
  }

  /**
    * Used to get part of ArrayList
    * Time complexity is O(n)
    * @example
    *          {{{
    *            val a = ArrayList(1, 2, 3, 4)
    *            a.subArray(1, 2) // returns ArrayList(2, 3)
    *            a.subArray(0, 3) // returns ArrayList(1, 2, 3)
    *            a.subArray(-1, 2) // returns ArrayList(1, 2)
    *            a.subArray(10, 10) // returns ArrayList()
    *            a.subArray(0, 0) //returns ArrayList()
    *          }}}
    * @param from where part should start
    * @param count how many elements it must contain
    * @return new ArrayList that 'count' contain elements from index 'from'
    */
  def subArray(from: Int, count: Int): ArrayList[A] = skip(from).take(count)

  /**
    * Used to get new ArrayList that contains only unique elements of this ArrayList
    * Time complexity is O(n). Space complexity is O(n)
    * @example
    *          {{{
    *            val a = ArrayList(1, 1, 1, 1, 1, 1, 2, 3, 3, 3, 3)
    *            a.distinct // returns ArrayList(1, 2, 3)
    *            ArrayList[Int]().distinct // returns ArrayList()
    *          }}}
    * @return new ArrayList with unique elements of this ArrayList
    */
  def distinct: ArrayList[A] = {
    val a = mutable.HashSet[A]()
    val result = ArrayList.withCapacity[A](length)
    for (i <- 0 until length) {
      if (a.add(this(i))) {
        result :+ this(i)
      }
    }
    result
  }

  /**
    * Used to get intersection between two ArrayLists
    * Time complexity is O(m + n). Space complexity is O(n)
    * @note Note that duplicates are removed
    * @example
    *          {{{
    *            val a = ArrayList(1, 2, 3)
    *            val b = ArrayList(2, 3, 4)
    *            a intersect b // returns ArrayList(2, 3)
    *            val c = ArrayList(1, 1, 1, 2)
    *            c intersect a // returns ArrayList(1, 2)
    *          }}}
    * @param that collection with which will be created intersection
    * @return new ArrayList that is intersection between this and that ArrayLists
    */
  def intersect(that: Collection[A]): ArrayList[A] = {
    val a = mutable.HashSet[A]()
    val result = ArrayList.withCapacity[A](length)
    foreach(x => a.add(x))
    that.foreach { x =>
      if (a.contains(x)) result :+ x
    }
    result
  }

}

/**
  * Companion object of ArrayList class.
  * Used to create ArrayLists.
  */
object ArrayList {

  final val defaultCapacity = 16
  protected final val growthFactor = 1.5

  /**
    * Creates new ArrayList with specified capacity
    * If capacity less than 0 - returns ArrayList with [[defaultCapacity]]
    * Time complexity is O(1)
    * @example
    *          {{{
    *            val a = ArrayList.withCapacity[Int](20) // Creates ArrayList of Int with capacity = 20
    *            val b = ArrayList.withCapacity[String](40) // Creates ArrayList of String with capacity = 40
    *            val c = ArrayList.withCapacity[Int](-10) // Creates ArrayList of Int with default capacity
    *          }}}
    * @param capacity Capacity of ArrayList
    * @tparam A Type of elements in ArrayList
    * @return new ArrayList with specified capacity
    */
  def withCapacity[A](capacity: Int): ArrayList[A] =
    if (capacity <= 0) new ArrayList[A](defaultCapacity) else new ArrayList[A](capacity)

  /**
    * Creates new ArrayList with specified elements.
    * Capacity of created ArrayList is count of passing elements.
    * Time complexity is O(n)
    * @example
    *          {{{
    *            val a = ArrayList(1, 2, 3, 4, 5) // Creates ArrayList of Int with elements 1, 2, 3, 4, 5
    *            val b = ArrayList("1", "2", "3") // Creates ArrayList of String with elements "1", "2", "3"
    *          }}}
    * @param args elements that should be added to ArrayList
    * @tparam A type of elements in ArrayList
    * @return new ArrayList with specified elements
    */
  def apply[A](args: A*): ArrayList[A] =
    if (args.isEmpty) empty[A]
    else args.foldLeft(new ArrayList[A](args.length)) {
    case (r, a) => r :+ a
  }

  /**
    * Creates new empty ArrayList with [[defaultCapacity]]
    * Time complexity is O(1)
    * @tparam A type of elements in ArrayList
    * @return new empty ArrayList with [[defaultCapacity]]
    */
  def empty[A] = new ArrayList[A]()
}
