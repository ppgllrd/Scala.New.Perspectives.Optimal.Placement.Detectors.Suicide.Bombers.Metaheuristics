package opsbd

import java.io.File

object InputOutput {
  def fill[A](v: A, label: String): String = {
    val str = v.toString
    val width = 50
    val fill = (width - str.length) max 1
    return str + "".padTo(fill, ' ') + label
  }
}

case class PrintWriter(file: File) extends java.io.PrintWriter(file) {
  def println[A](v: A, label: String) { super.println(InputOutput.fill(v, label)) }
}

class Scanner(readable: Readable) {
  val scanner = new java.util.Scanner(readable)

  def this(str: String) { this(new java.io.StringReader(str)) }

  def this(file: File) { this(new java.io.FileReader(file)) }

  def nextIntLn(): Int = new java.util.Scanner(scanner.nextLine()).nextInt()

  def nextDoubleLn(): Double = new java.util.Scanner(scanner.nextLine()).nextDouble()

  def nextBooleanLn(): Boolean = new java.util.Scanner(scanner.nextLine()).nextBoolean()

  def nextInt(): Int = scanner.nextInt()

  def nextDouble(): Double = scanner.nextDouble()

  def nextLine(): String = scanner.nextLine()

  def close() { scanner.close() }
}

object Scanner {
  def apply(readable: Readable) = new Scanner(readable)

  def apply(str: String) = new Scanner(str)

  def apply(file: File) = new Scanner(file)
}
