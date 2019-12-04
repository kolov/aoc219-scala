package com.akolov.aoc2019

object Day4 extends App {

  def passes1(s: List[Char]): Boolean = {
    s.zip(s.tail).exists { case (a, b) => a == b }
  }

  def nextChar(c: Char) = (c.toInt + 1).toChar

  def next(s: List[Char]): List[Char] = {
    def increaseAt(pos: Int): List[Char] = {
      if (s(pos) < '9') {
        val newChar = nextChar(s(pos))
        s.take(pos) ++ List.fill(s.length - pos)(newChar)
      } else {
        if (pos > 0) {
          increaseAt(pos - 1)
        } else {
          List.fill(s.length + 1)('1')
        }
      }
    }
    increaseAt(s.length - 1)
  }

  def nextGood(s: List[Char]) = {
    val firstWrong = s
      .zip(s.tail)
      .map { case (a, b) => a < b }
      .takeWhile(a => a)
      .size + 1
    s.take(firstWrong) ++ List
      .fill(s.size - firstWrong)(s(firstWrong - 1))
  }

  def answer1(from: Int, to: Int) = {
    LazyList
      .iterate(nextGood(from.toString.toList))(next)
      .takeWhile(s => new String(s.toArray).toInt <= to)
      .filter(passes1)
      .toList
      .map(s => new String(s.toArray))
//      .size
  }

  println(s"Answer1 ${answer1(137683, 596253)}")

}
