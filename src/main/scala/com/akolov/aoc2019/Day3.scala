package com.akolov.aoc2019

import scala.io.Source

object Day3 extends App {

  def readInput(): List[List[String]] = {
    val source = Source
      .fromInputStream(getClass.getResourceAsStream("/input-day-3.txt"))
    val program = source.getLines.toList.map(l => l.split(",").toList)
    source.close()
    program
  }

  case class Path(pos: (Int, Int), visited: Map[(Int, Int), Int], time: Int) {
    def moveBy(offset: (Int, Int)) = {
      val newPos = (pos._1 + offset._1, pos._2 + offset._2)
      visited.get(newPos) match {
        case None => Path(newPos, visited.updated(newPos, time + 1), time + 1)
        case _    => Path(newPos, visited, time + 1)
      }
    }
  }

  def wirePoints(wire: List[String]) = {
    wire.foldLeft(Path((0, 0), Map.empty, 0)) {
      case (path, command) =>
        val len = command.substring(1).toInt
        val step = command.charAt(0) match {
          case 'U' => (0, 1)
          case 'D' => (0, -1)
          case 'R' => (1, 0)
          case 'L' => (-1, 0)
        }
        (1 to len).map(_ => step).foldLeft(path) {
          case (path, pos) => path.moveBy(pos)
        }
    }
  }

  def distance(p: (Int, Int)) = Math.abs(p._1) + Math.abs(p._2)

  val wires = readInput
  val path0 = wirePoints(wires(0))
  val path1 = wirePoints(wires(1))
  val crossings = path0.visited.keys.filter(path1.visited.contains)

  val answer1 = crossings
    .map(distance)
    .toList
    .sorted
    .head

  val answer2 = crossings
    .map(p => path0.visited.get(p).get + path1.visited.get(p).get)
    .toList
    .sorted
    .head

  println(s"Answer1 $answer1")
  println(s"Answer2 $answer2")

}
