package com.rockthejvm.lists

import javax.sql.rowset.Predicate
import scala.annotation.tailrec
import scala.jdk.Accumulator
import scala.util.Random

sealed abstract class RList[+T]{
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)
  def apply(index: Int): T
  def length: Int
  def reverse: RList[T]
  def ++[S >: T](anotherList: RList[S]) : RList[S]
  def remove(index: Int): RList[T]
  def map[S](f: T => S): RList[S]
  def flatMap[S](f: T => RList[S]): RList[S]
  def filter(f: T => Boolean): RList[T]
  def rle: RList[(T, Int)]
  def duplicateEach(n: Int): RList[T]
  def rotate(n: Int): RList[T]
  def sample(n: Int): RList[T]
}

case object RNil extends RList[Nothing]{
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def toString: String = "[]"
  override def apply(index: Int): Nothing = throw new NoSuchElementException
  override def length: Int = 0
  override def reverse: RList[Nothing] = RNil
  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = RNil
  override def remove(index: Int): RList[Nothing] = RNil
  override def map[S](f: Nothing => S): RList[S] = RNil
  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil
  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil
  override def rle: RList[(Nothing, Int)] = RNil
  override def duplicateEach(n: Int): RList[Nothing] = RNil
  override def rotate(n: Int): RList[Nothing] = RNil
  override def sample(n: Int): RList[Nothing] = RNil
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false
  override def toString: String = {
    @tailrec
    def toStringTailrec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailrec(remaining.tail, s"$result${remaining.head}, ")
    }

    "[" + toStringTailrec(this, "") + "]"
  }
  override def apply(index: Int): T = {
    @tailrec
    def applyTailrec(remaining: RList[T], currentIndex: Int): T = {
      if(currentIndex == index) remaining.head
      else applyTailrec(remaining.tail, currentIndex + 1)
    }
    if(index < 0) throw new NoSuchElementException
    else applyTailrec(this, 0)
  }
  override def length: Int = {
    @tailrec
    def lengthTailrec(remaining: RList[T], count: Int): Int = {
      if(remaining.isEmpty) count
      else lengthTailrec(remaining.tail, count + 1)
    }
    lengthTailrec(this, 0)
  }
  override def reverse: RList[T] = {

    @tailrec
    def reverseTailrec(remaining: RList[T], result: RList[T]): RList[T] = {
      if(remaining.isEmpty) result
      else reverseTailrec(remaining.tail, remaining.head :: result)
    }
    reverseTailrec(this,RNil)
  }
  override def ++[S >: T](anotherList: RList[S]): RList[S] = {
    @tailrec
    def appendTailRec(remaining: RList[S], acc: RList[S]): RList[S] = {
      if(remaining.isEmpty) acc
      else appendTailRec(remaining.tail,remaining.head :: acc)
    }
    appendTailRec(anotherList, this.reverse).reverse
  }
  override def remove(index: Int): RList[T] = {
    /*
    [ 1, 2, 3 ] 2
    removeTailRec( 0 , [1,2,3], [] )
     */
    @tailrec
    def removeTailRec(currentIndex: Int, remaining: RList[T], acc: RList[T]): RList[T] = {
      if(currentIndex == index) acc.reverse ++ remaining.tail
      else if (remaining.isEmpty) acc.reverse
      else removeTailRec(currentIndex + 1, remaining.tail, remaining.head :: acc)
    }
    removeTailRec(0,this,RNil)
  }
  override def map[S](f: T => S): RList[S] = {
    @tailrec
    def mapTailrec(remaining: RList[T], acc: RList[S]): RList[S] = {
      if(remaining.isEmpty) acc
      else mapTailrec(remaining.tail,f(remaining.head) :: acc)
    }
    mapTailrec(this,RNil).reverse
  }
  override def flatMap[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def flatMapTailrec(remaining: RList[T], acc: RList[S]): RList[S] = {
      if(remaining.isEmpty) acc.reverse
      else flatMapTailrec(remaining.tail, f(remaining.head) ++ acc)
    }
    flatMapTailrec(this,RNil)
  }
  override def filter(predicate: T => Boolean): RList[T] = {
    @tailrec
    def filterTailRec(remaining: RList[T], accumulator: RList[T]): RList[T] = {
      if(remaining.isEmpty) accumulator.reverse
      else if(predicate(remaining.head)) filterTailRec(remaining.tail, remaining.head :: accumulator)
      else filterTailRec(remaining.tail, accumulator)
    }
    filterTailRec(this,RNil)
  }
  override def rle: RList[(T, Int)] = {
    @tailrec
    def rleTailRec(remaining: RList[T], curr: (T, Int), acc: RList[(T, Int)]): RList[(T, Int)] = {
      if(remaining.isEmpty && curr._2 == 0) acc
      else if(remaining.isEmpty) curr :: acc
      else if(remaining.head == curr._1) rleTailRec(remaining.tail,curr.copy(_2 = curr._2 + 1), acc)
      else rleTailRec(remaining.tail, (remaining.head, 1), curr :: acc)
    }
    rleTailRec(this.tail,(this.head,1),RNil).reverse
  }
  override def duplicateEach(n: Int): RList[T] = {

    @tailrec
    def duplicateTailrec(remaining: RList[T], count: Int, acc: RList[T]): RList[T] = {
      if(remaining.isEmpty) acc.reverse
      else if(count > 1) duplicateTailrec(remaining, count - 1, remaining.head :: acc)
      else duplicateTailrec(remaining.tail, n, remaining.head :: acc)
    }
    duplicateTailrec(this,n,RNil)
  }
  override def rotate(n: Int): RList[T] = {
    @tailrec
    def rotateTailrec(remaining: RList[T], front: RList[T], i: Int, back: RList[T]): RList[T] = {
      if(remaining.isEmpty) back.reverse ++ front.reverse
      else if(i < n) rotateTailrec(remaining.tail, remaining.head :: front, i + 1, back)
      else rotateTailrec(remaining.tail, front, i + 1, remaining.head :: back)
    }
    rotateTailrec(this, RNil, 0, RNil)
  }

  override def sample(n: Int): RList[T] = {
    val random = new Random(System.currentTimeMillis())
    val maxLen = this.length
    @tailrec
    def sampleTailrec(nRemaining: Int, acc: RList[T]): RList[T] = {
      if(nRemaining == 0) acc.reverse
      else {
        val i = random.nextInt(maxLen)
        val newNumber = this(i)
        sampleTailrec(nRemaining - 1, newNumber :: acc)
      }
    }
    if(n < 0) RNil
    else sampleTailrec(n, RNil)
  }
}

object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {

    @tailrec
    def convertToRListTailrec(remaining: Iterable[T], accumulator: RList[T]): RList[T] = {
      if(remaining.isEmpty) accumulator
      else convertToRListTailrec(remaining.tail, remaining.head :: accumulator)
    }
    convertToRListTailrec(iterable,RNil).reverse
  }
}

object ListProblems extends App {
  val aSmallList = 1 :: 2 :: 3 :: RNil
  val aLargeList = RList.from(1 to 10)

//  println(aSmallList)
//  println(aSmallList.filter(x => x % 2 == 1))
//  println(aSmallList.duplicateEach(3))
  println(aLargeList.rotate(3))
  println(aLargeList.sample(3))
}
