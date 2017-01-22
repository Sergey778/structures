package structures.mutable

import scala.util.Failure

class ArrayListTest extends org.scalatest.FlatSpec with org.scalatest.Matchers {

  "ArrayList" can "be created on call of 'apply', 'empty', 'withCapacity' methods of companion object" in {
    val nonEmptyList = ArrayList(1, 2, 3, 4, 5, 6, 7)
    nonEmptyList.size shouldEqual 7
    nonEmptyList.capacity shouldEqual 7

    val emptyList = ArrayList[Int]()
    emptyList.capacity shouldEqual ArrayList.defaultCapacity
    emptyList.isEmpty shouldEqual true

    val explicitEmptyList = ArrayList.empty[Int]
    explicitEmptyList.capacity shouldEqual ArrayList.defaultCapacity
    explicitEmptyList.isEmpty shouldEqual true

    val incorrectCapacityList = ArrayList.withCapacity[Int](ArrayList.defaultCapacity)
    incorrectCapacityList.capacity shouldEqual ArrayList.defaultCapacity
    incorrectCapacityList.isEmpty shouldEqual true
  }

  "ArrayList capacity" can "be changed with 'reserve' and 'tryReserve' methods" in {
    val a = ArrayList[Int](1, 2, 3, 4, 5, 6, 7, 8)
    a.capacity shouldEqual 8
    a.reserve(40)
    a.capacity shouldEqual 40
    a.size shouldEqual 8
    a.tryReserve(-1).isFailure shouldEqual true
    a.reserve(-1).capacity shouldEqual 40
    a.tryReserve(16).isSuccess shouldEqual true
    val b = a.reserve(4)
    b.size shouldEqual 4
    b.capacity shouldEqual 4
  }

  "'insert' method" must "insert element in ArrayList" in {
    val a = ArrayList(0, 1, 2, 3, 4, 5, 6, 7)
    a.insert(3, -11) // 0, 1, 2, -11, 3, 4, 5, 6, 7
    a(3) shouldEqual -11
    a.insert(5, 165) // 0, 1, 2, -11, 3, 165, 4, 5, 6, 7
    a(5) shouldEqual 165
    a.insert(0, 1111) // 1111, 0, 1, 2, -11, 3, 165, 4, 5, 6, 7
    a(0) shouldEqual 1111
    a.insert(7, 1111) // 1111, 0, 1, 2, -11, 3, 165, 1111, 4, 5, 6, 7
    a(7) shouldEqual 1111
    a shouldEqual ArrayList(1111, 0, 1, 2, -11, 3, 165, 1111, 4, 5, 6, 7)
  }

  it should "do nothing if index is incorrect" in {
    val a = ArrayList(0, 1, 2, 3)
    val b = ArrayList(0, 1, 2, 3)
    a.insert(-1, -1)
    a shouldEqual b
    a.insert(4, -1)
    a shouldEqual b
  }

  "'tryInsert' method" must "insert element in ArrayList and return Success" in {
    val a = ArrayList(0, 1, 2, 3, 4, 5, 6, 7)
    a.tryInsert(3, -11).isSuccess shouldEqual true
    a(3) shouldEqual -11
    a.tryInsert(5, 165).isSuccess shouldEqual true
    a(5) shouldEqual 165
    a.tryInsert(0, 1111).isSuccess shouldEqual true
    a(0) shouldEqual 1111
    a.tryInsert(7, 1111).isSuccess shouldEqual true
    a(7) shouldEqual 1111
    a shouldEqual ArrayList(1111, 0, 1, 2, -11, 3, 165, 1111, 4, 5, 6, 7)
  }

  it should "return Failure if index is incorrect" in {
    val a = ArrayList(0, 1, 2, 3)
    val b = ArrayList(0, 1, 2, 3)
    a.tryInsert(-1, -1).isFailure shouldEqual true
    a shouldEqual b
    a.tryInsert(4, -1).isFailure shouldEqual true
    a shouldEqual b
  }

  "'+:' method" must "add element to front of ArrayList" in {
    val a = ArrayList[Int]()
    val b = ArrayList(3, 2, 1)
    3 +: 2 +: 1 +: a
    a shouldEqual b
    4 +: b
    b(0) shouldEqual 4
    5 +: b
    b(1) shouldEqual 4
    b(0) shouldEqual 5
  }

  "'removeAt' method" must "remove element from ArrayList" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6, 7)
    a.removeAt(0)
    a(0) shouldEqual 2
    a.removeAt(a.size - 1)
    a(a.size - 1) shouldEqual 6
    a.removeAt(3)
    a shouldEqual ArrayList(2, 3, 4, 6)
  }

  it should "do nothing if index is incorrect" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6, 7)
    val b = ArrayList(1, 2, 3, 4, 5, 6, 7)
    a.removeAt(-1)
    a shouldEqual b
    a.removeAt(8)
    a shouldEqual b
  }

  "'tryRemoveAt' method" must "remove element from ArrayList and return Success" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6, 7)
    a.tryRemoveAt(0).isSuccess shouldEqual true
    a(0) shouldEqual 2
    a.tryRemoveAt(a.size - 1).isSuccess shouldEqual true
    a(a.size - 1) shouldEqual 6
    a.tryRemoveAt(3).isSuccess shouldEqual true
    a shouldEqual ArrayList(2, 3, 4, 6)
  }

  it should "return Failure if index is incorrect" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6, 7)
    val b = ArrayList(1, 2, 3, 4, 5, 6, 7)
    a.tryRemoveAt(-1).isFailure shouldEqual true
    a shouldEqual b
    a.tryRemoveAt(8).isFailure shouldEqual true
    a shouldEqual b
  }

  "'update' method" must "update value of element on passed index in ArrayList" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6, 7, 8)
    a.update(0, 11)
    a(0) shouldEqual 11
    a.update(7, 56)
    a(7) shouldEqual 56
    a.update(4, 101)
    a(4) shouldEqual 101
    a shouldEqual ArrayList(11, 2, 3, 4, 101, 6, 7, 56)
  }

  it should "do nothing if index is incorrect" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6, 7, 8)
    val b = ArrayList(1, 2, 3, 4, 5, 6, 7, 8)
    a.update(-2, 11)
    a(0) shouldEqual 1
    a.update(8, 23)
    a(7) shouldEqual 8
    a shouldEqual b
  }

  "'tryUpdate' method" must "update value of element on passed index in ArrayList and return Success" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6, 7, 8)
    a.tryUpdate(0, 11).isSuccess shouldEqual true
    a(0) shouldEqual 11
    a.tryUpdate(7, 56).isSuccess shouldEqual true
    a(7) shouldEqual 56
    a.tryUpdate(4, 101).isSuccess shouldEqual true
    a(4) shouldEqual 101
    a shouldEqual ArrayList(11, 2, 3, 4, 101, 6, 7, 56)
  }

  it should "return Failure if index is incorrect" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6, 7, 8)
    val b = ArrayList(1, 2, 3, 4, 5, 6, 7, 8)
    a.tryUpdate(-2, 11).isFailure shouldEqual true
    a(0) shouldEqual 1
    a.tryUpdate(8, 23).isFailure shouldEqual true
    a(7) shouldEqual 8
    a shouldEqual b
  }

  "'size' method" must "return count of elements in ArrayList" in {
    val a = ArrayList[Int]()
    a.size shouldEqual 0
    a :+ 1 :+ 2
    a.size shouldEqual 2
    a :+ 3
    a.size shouldEqual 3
    a.removeAt(0)
    a.size shouldEqual 2
  }

  "'capacity' method" must "return count of elements that ArrayList can store without resizing" in {
    val a = ArrayList[Int]()
    a.capacity shouldEqual ArrayList.defaultCapacity
    a.reserve(64)
    a.capacity shouldEqual 64
    a.reserve(4)
    a.capacity shouldEqual 4
    a :+ 1 :+ 2 :+ 3 :+ 4
    a.capacity shouldEqual 4
  }

  "'foreach' method" must "execute passing function on every element of ArrayList" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6, 7, 8)
    val b = ArrayList[Int]()
    a.foreach(i => b :+ i)
    b shouldEqual a
  }

  it should "return same ArrayList" in {
    val a = ArrayList(1, 2, 3, 4)
    a.foreach(x => x + 2) eq a shouldEqual true
  }

  "'filter' method" must "return elements of ArrayList that satisfies passing predicate" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6, 7, 8)
    val b = a.filter(_ % 2 == 0)
    b shouldEqual ArrayList(2, 4, 6, 8)
    val c = a.filter(_ => true)
    c shouldEqual a
  }

  it must "return new ArrayList" in {
    val a = ArrayList(1, 2, 3, 4)
    a.filter(_ => true) eq a shouldEqual false
  }

  "'foldLeft' method" must "aggregate on all element of ArrayList from left to right" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6, 7)
    a.foldLeft(0)(_ + _) shouldEqual 1 + 2 + 3 + 4 + 5 + 6 + 7
    a.foldLeft(100000)(_ / _) shouldEqual 100000 / 1 / 2 / 3 / 4 / 5 / 6 / 7
    a.foldLeft(ArrayList[Int]())((r, c) => r :+ c) shouldEqual a
  }

  "'foldRight' method" must "aggregate on all element of ArrayList from right to left" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6, 7)
    a.foldRight(0)(_ + _) shouldEqual 1 + 2 + 3 + 4 + 5 + 6 + 7
    a.foldRight(100000)(_ / _) shouldEqual 100000 / 7 / 6 / 5 / 4 / 3 / 2 / 1
    a.foldRight(ArrayList[Int]())((r, c) => r :+ c) shouldEqual ArrayList(7, 6, 5, 4, 3, 2, 1)
  }

  "'take' method" must "return passing count of elements from start of ArrayList" in {
    val a = ArrayList(1, 2, 3, 4, 5)
    a.take(3) shouldEqual ArrayList(1, 2, 3)
    a.take(5) shouldEqual ArrayList(1, 2, 3, 4, 5)
    a.take(a.size) shouldEqual a
  }

  it should "return empty ArrayList if count <= 0" in {
    val a = ArrayList(1, 2, 3, 4, 5)
    a.take(0) shouldEqual ArrayList.empty[Int]
    a.take(-1).isEmpty shouldEqual true
  }

  it should "return copy of ArrayList if count > size" in {
    val a = ArrayList(1, 2, 3, 4, 5)
    a.take(1000) shouldEqual a
    a.take(1000) eq a shouldEqual false
  }

  "'splitAt' method" must "split ArrayList to two ArrayLists" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6, 7, 8)
    val (f, s) = a.splitAt(3)
    f shouldEqual ArrayList(1, 2, 3)
    s shouldEqual ArrayList(4, 5, 6, 7, 8)
    val (b, c) = a.splitAt(6)
    b shouldEqual ArrayList(1, 2, 3, 4, 5, 6)
    c shouldEqual ArrayList(7, 8)
  }

  it should "return copy of ArrayList and empty ArrayList if index >= length" in {
    val a = ArrayList(1, 2, 3, 4, 5)
    val (f, s) = a.splitAt(10)
    f shouldEqual a
    s shouldEqual ArrayList.empty[Int]
  }

  it should "return empty ArrayList and copy of ArrayList if index < 0" in {
    val a = ArrayList(1, 2, 3, 4, 5)
    val (f, s) = a.splitAt(-1)
    f shouldEqual ArrayList.empty[Int]
    s shouldEqual a
  }

  "'iterator' method" must "return iterator of ArrayList" in {
    val a = ArrayList(1, 2, 3, 4, 5)
    val iterator = a.iterator
    for (i <- a) {
      i shouldEqual iterator.next()
    }
  }

  it should "return empty iterator if ArrayList is empty" in {
    val a = ArrayList.empty[Int]
    a.iterator.isEmpty shouldEqual true
  }

  "'headOption' method" must "return Some with first element of ArrayList" in {
    val a = ArrayList(1, 2, 3)
    a.headOption.nonEmpty shouldEqual true
    a.headOption.get shouldEqual 1
    val b = ArrayList(args = Seq(1):_*)
    b.headOption.nonEmpty shouldEqual true
    b.headOption.get shouldEqual 1
  }

  it should "return None if ArrayList is empty" in {
    val a = ArrayList.empty[Int]
    a.headOption.isEmpty shouldEqual true
  }

  "'apply' method" must "return element on passing index in ArrayList" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6, 7, 8)
    a(0) shouldEqual 1
    a(4) shouldEqual 5
  }

  it must "throw exception if index is incorrect" in {
    val a = ArrayList(1, 2, 3, 4)
    intercept[ArrayIndexOutOfBoundsException] {
      a(5)
    }
    intercept[ArrayIndexOutOfBoundsException] {
      a(-1)
    }
  }

  "'tail' method" must "return tail of ArrayList" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6)
    a.tail shouldEqual ArrayList(2, 3, 4, 5, 6)
    a.tail.tail shouldEqual ArrayList(3, 4, 5, 6)
    a.tail.tail.tail shouldEqual a.skip(3)
  }

  it must "return empty ArrayList if length <= 1" in {
    val a = ArrayList("abc")
    a.tail shouldEqual ArrayList.empty[String]
    val b = ArrayList("abc", "bcd")
    b.tail.tail shouldEqual ArrayList.empty[String]
    val c = ArrayList.empty[Int]
    c.tail shouldEqual ArrayList.empty[Int]
  }

  "'skip' method" must "return ArrayList without first n elements" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6, 7)
    a.skip(3) shouldEqual ArrayList(4, 5, 6 ,7)
    a.skip(7) shouldEqual ArrayList.empty[Int]
  }

  it must "return full ArrayList if n <= 0" in {
    val a = ArrayList(1, 2, 3, 4, 5)
    a.skip(0) shouldEqual a
    a.skip(-1) shouldEqual a
  }

  it must "return empty ArrayList if n >= length" in {
    val a = ArrayList(1, 2, 3, 4, 5)
    a.skip(a.size) shouldEqual ArrayList.empty[Int]
    a.skip(9000) shouldEqual ArrayList.empty[Int]
  }

  "':+' method" must "append element to the end of ArrayList" in {
    val a = ArrayList(1, 2)
    a :+ 3
    a(2) shouldEqual 3
    a :+ 4
    a(3) shouldEqual 4
    a :+ 5 :+ 6 shouldEqual ArrayList(1, 2, 3, 4, 5, 6)
  }

  "'+' method" must "append element to the end of ArrayList" in {
    val a = ArrayList(1, 2)
    a + 3
    a(2) shouldEqual 3
    a + 4
    a(3) shouldEqual 4
    a + 5 + 6 shouldEqual ArrayList(1, 2, 3, 4, 5, 6)
  }

  "'map' method" must "transform elements of ArrayList with help of passing function" in {
    val a = ArrayList(1, 2, 3, 4)
    a.map(_ * 2) shouldEqual ArrayList(2, 4, 6, 8)
    a.map(_.toString) shouldEqual ArrayList("1", "2", "3", "4")
  }

  it must "return new ArrayList" in {
    val a = ArrayList(1, 2, 3, 4)
    a.map(x => x) eq a shouldEqual false
  }

  "'clear' method" must "make ArrayList empty" in {
    val a = ArrayList(1, 2, 3, 4, 5)
    a.clear()
    a.size shouldEqual 0
    a.isEmpty shouldEqual true
    val b = ArrayList.empty[String]
    b.clear()
    b.isEmpty shouldEqual true
  }

  "'equals' method" must "return true if arrays are equal" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6)
    val b = ArrayList(1, 2, 3, 4, 5, 6)
    a shouldEqual b
  }

  it must "return false if arrays are not equal" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6)
    val b = ArrayList(1, 2, 3, 4, 5)
    a should not equal b
    val c = ArrayList(1, 2, 3, 4, 5, 7)
    a should not equal c
  }

  it must "return false if other argument is not same type" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6)
    val b = ArrayList("a", "b", "c", "d", "e", "f")
    a should not equal b
    val c = List(1, 2, 3, 4)
    a should not equal c
    val d = new AnyRef { val a: String = ""}
    a should not equal d
  }

  "'head' method" must "return first element of ArrayList" in {
    val a = ArrayList(1, 2, 3, 4)
    a.head shouldEqual 1
    a.tail.head shouldEqual 2
  }

  it must "throw exception on empty ArrayList" in {
    intercept[UnsupportedOperationException] {
      ArrayList.empty[Int].head
    }
  }

  "'min' method" must "return the smallest element of ArrayList" in {
    val a = ArrayList(1, 2, 3, 4, 5)
    a.min shouldEqual 1
    val b = ArrayList("a", "b", "c")
    b.min shouldEqual "a"
  }

  it should "work with explicit Ordering" in {
    class Person(val name: String)
    val a = new Person("abc")
    val b = new Person("cdb")
    val list = ArrayList(a, b)
    val ord = new Ordering[Person] {
      override def compare(x: Person, y: Person): Int = x.name.compareTo(y.name)
    }
    list.min(ord) shouldEqual a
  }

  "'max' method" must "return the smallest element of ArrayList" in {
    val a = ArrayList(1, 2, 3, 4, 5)
    a.max shouldEqual 5
    val b = ArrayList("a", "b", "c")
    b.max shouldEqual "c"
  }

  it should "work with explicit Ordering" in {
    class Person(val name: String)
    val a = new Person("abc")
    val b = new Person("cdb")
    val list = ArrayList(a, b)
    val ord = new Ordering[Person] {
      override def compare(x: Person, y: Person): Int = x.name.compareTo(y.name)
    }
    list.max(ord) shouldEqual b
  }

  "'isEmpty' and 'nonEmpty' methods" must "return false and true respectively if ArraList is not empty" in {
    ArrayList(1, 2, 3, 4).nonEmpty shouldEqual true
    ArrayList(1, 2, 3, 4).isEmpty shouldEqual false
  }

  it must "return true and false respectively if ArrayList is empty" in {
    ArrayList.empty[Int].nonEmpty shouldEqual false
    ArrayList.empty[Int].isEmpty shouldEqual true
  }

  "'filterNot' method" must "return elements that not satisfy predicate in ArrayList" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6)
    a.filterNot(_ % 2 == 0) shouldEqual ArrayList(1, 3, 5)
    a.filterNot(_ => true) shouldEqual ArrayList.empty[Int]
  }

  "'toString' method" must "return string representation of ArrayList" in {
    val a = ArrayList(1, 2, 3)
    a.toString shouldEqual "ArrayList(1, 2, 3)"
    val b = ArrayList(1)
    b.toString shouldEqual "ArrayList(1)"
    val c = ArrayList.empty[Int]
    c.toString shouldEqual "ArrayList()"
    val d = ArrayList(ArrayList(1), ArrayList(2))
    d.toString shouldEqual "ArrayList(ArrayList(1), ArrayList(2))"
  }

  "'subArray' method" must "return part of ArrayList" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6)
    a.subArray(0, 4) shouldEqual ArrayList(1, 2, 3, 4)
    a.subArray(4, 2) shouldEqual ArrayList(5, 6)
    a.subArray(2, 4) shouldEqual ArrayList(3, 4, 5, 6)
    a.subArray(2, 3) shouldEqual ArrayList(3, 4, 5)
    a.subArray(0, 0) shouldEqual ArrayList[Int]()
    a.subArray(0, 6) shouldEqual a
  }

  it must "return empty ArrayList if from is bigger than length" in {
    val a = ArrayList(1, 2, 3)
    a.subArray(4, 1) shouldEqual ArrayList.empty[Int]
    a.subArray(11, 100) shouldEqual ArrayList.empty[Int]
    ArrayList[Int]().subArray(0, 1) shouldEqual ArrayList.empty[Int]
  }

  it must "return full ArrayList if count is bigger than length" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6)
    a.subArray(0, 10) shouldEqual a
  }

  it must "return empty ArrayList if count is 0 or less" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6)
    a.subArray(0, -1) shouldEqual ArrayList.empty[Int]
    a.subArray(0, 0) shouldEqual ArrayList.empty[Int]
  }

  it must "behave like from is 0 when from is negate" in {
    val a = ArrayList(1, 2, 3, 4, 5, 6)
    a.subArray(-10, 6) shouldEqual a
  }

  "'distinct' method" must "return only unique elements of ArrayList" in {
    val a = ArrayList(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3)
    a.distinct shouldEqual ArrayList(1, 2, 3)
    ArrayList[Int]() shouldEqual ArrayList[Int]()
    ArrayList(1, 2, 3).distinct shouldEqual ArrayList(1, 2, 3)
  }

  "'intersect' method" must "return shared elements between two ArrayLists" in {
    val a = ArrayList(1, 2, 3, 4)
    val b = ArrayList(4, 5, 6, 7)
    a intersect b shouldEqual ArrayList(4)
    ArrayList[Int]() intersect ArrayList[Int]() shouldEqual ArrayList[Int]()

    val c = ArrayList(1, 2, 3)
    c intersect c shouldEqual c
  }
}
