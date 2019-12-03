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
    def nextPos(pos: (Int, Int)) = {
      visited.get(pos) match {
        case None    => Path(pos, visited.updated(pos, time + 1), time + 1)
        case Some(t) => Path(pos, visited.updated(pos, t), time + 1)
      }
    }
  }

  val wires = readInput

  def wirePoints(wire: List[String]) = {
    wire.foldLeft(Path((0, 0), Map.empty, 0)) {
      case (path @ Path((x, y), visited, time), command) =>
        val len = command.substring(1).toInt
        val steps = (1 to len)
        val positions = command.charAt(0) match {
          case 'U' => steps.map(i => (x, y + i))
          case 'D' => steps.map(i => (x, y - i))
          case 'R' => steps.map(i => (x + i, y))
          case 'L' => steps.map(i => (x - i, y))
        }
        positions.foldLeft(path){ case (path, pos) => path.nextPos(pos)}
    }
  }

  def distance(p: (Int, Int)) = Math.abs(p._1) + Math.abs(p._2)

  val path0 = wirePoints(wires(0))
  val path1 = wirePoints(wires(1))

  private val crossings  = path0.visited.keys.filter(path1.visited.contains)

  val answer1 = crossings
    .map(distance)
    .toList
    .sorted
    .head

  val answer2 = crossings
    .map(p =>  path0.visited.get(p).get + path1.visited.get(p).get)
    .toList
    .sorted
    .head
  println(s"Answer2 $answer1")
  println(s"Answer2 $answer2")

}
