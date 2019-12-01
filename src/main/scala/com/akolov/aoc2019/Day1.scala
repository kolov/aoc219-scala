package com.akolov.aoc2019

import scala.io.Source

object Day1 extends App {

  def fuelSimple(mass: Int): Int = (mass / 3) - 2
  def fuelCompound(mass: Int): Int = {
    val f = fuelSimple(mass)
    if (f < 0)
      0
    else
      f + fuelCompound(f)
  }

  def readModules() = {
    val source = Source
      .fromInputStream(getClass.getResourceAsStream("/input-day-1.txt"))
    val modules = source.getLines.toList.map(_.toInt)
    source.close()
    modules
  }

  val modules = readModules

  println(s"Total simple: ${modules.map(fuelSimple).sum}")
  println(s"Total compound: ${modules.map(fuelCompound).sum}")

}
