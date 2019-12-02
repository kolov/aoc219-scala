package com.akolov.aoc2019

import scala.io.Source

object Day2 extends App {

  type P = Vector[Int]

  def readProgram(): P = {
    val source = Source
      .fromInputStream(getClass.getResourceAsStream("/input-day-2.txt"))
    val program = source.getLines.mkString("").split(",").map(_.toInt).toVector
    source.close()
    program
  }

  val program = readProgram

  def executeAt(pos: Int, p: P): (Boolean, P) = {

    def read(pos: Int) = p(p(pos))
    def write(pos: Int, value: Int) = p.updated(p(pos), value)

    p(pos) match {
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

  def patch(p: P, noun: Int, verb: Int): P =
    p.updated(1, noun).updated(2, verb)

  val answer1 = {
    val e = execute(0, patch(program, 12, 2))
    e(0)
  }

  val answer2 = {
    LazyList
      .from(1, 1)
      .map { i =>
        val result = execute(0, patch(program, i / 100, i % 100))
        (i, result(0))
      }
      .filter {
        case (_, r) => r == 19690720
      }
      .headOption
  }
  println(s"Answer1 $answer1")
  println(s"Answer2 $answer2")

}
