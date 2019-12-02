package com.akolov.aoc2019

import scala.io.Source

object Day2 extends App {

  def readProgram() = {
    val source = Source
      .fromInputStream(getClass.getResourceAsStream("/input-day-2.txt"))
    val program = source.getLines.mkString("").split(",").map(_.toInt).toList
    source.close()
    program
  }

  val program = readProgram

  type P = Map[Int, Int]

  def executeAt(pos: Int, p: P): (Boolean, P) = {

    def read(pos: Int) = p.get(p.get(pos).get).get
    def write(pos: Int, value: Int) = p.updated(p.get(pos).get, value)

    p.get(pos).get match {
      case 1 =>
        (false, write(pos + 3, read(pos + 1) + read(pos + 2)))
      case 2 =>
        (false, write(pos + 3, read(pos + 1) * read(pos + 2)))
      case 99 =>
        (true, p)
    }
  }

  def execute(pos: Int, p: P): P = {
    val (finished, next) = executeAt(pos, p)
    if (finished)
      next
    else
      execute(pos + 4, next)
  }

  def toMap(l: List[Int]) = l.zipWithIndex.map { case (a, b) => (b, a) }.toMap

  def executeList(l: List[Int]) = {
    execute(0, toMap(l))
  }

  def patch(p: P, noun: Int, verb: Int): P =
    p.updated(1, noun).updated(2, verb)

  val answer1 = {
    execute(0, patch(toMap(program), 12, 2)).get(0).get
  }

  val answer2 = {
    val m = toMap(program)
    LazyList
      .from(1, 1)
      .map { i =>
        val program = patch(m, i / 100, i % 100)
        val result = execute(0, program)
        (i, result.get(0).get)
      }
      .filter {
        case (_, r) => r == 19690720
      }
      .headOption
  }
  println(s"Answer1 $answer1")
  println(s"Answer2 $answer2")

}
