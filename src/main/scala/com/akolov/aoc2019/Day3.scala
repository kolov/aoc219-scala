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

  case class Path(pos: (Int, Int), visited: Set[(Int, Int)])
  val wires = readInput

  def wirePoints(wire: List[String]) = {
    wire.foldLeft(Path((0, 0), Set.empty)) {
      case (Path((x, y), visited), command) =>
        val len = command.substring(1).toInt
        command.charAt(0) match {
          case 'U' =>
            Path((x, y + len), visited ++ (1 to len).map { i =>
              (x, y + i)
            }.toSet)
          case 'D' =>
            Path((x, y - len), visited ++ (1 to len).map { i =>
              (x, y - i)
            }.toSet)
          case 'R' =>
            Path((x + len, y), visited ++ (1 to len).map { i =>
              (x + i, y)
            }.toSet)
          case 'L' =>
            Path((x - len, y), visited ++ (1 to len).map { i =>
              (x - i, y)
            }.toSet)
        }
    }
  }

  def distance(p: (Int, Int)) = Math.abs(p._1) + Math.abs(p._2)

  val path0 = wirePoints(wires(0))
  val path1 = wirePoints(wires(1))

  val answer1 = path0.visited.filter(path1.visited.contains).map(distance).toList.sorted.head
  println(s"Answer2 $answer1")

}
