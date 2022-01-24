``` scala
import scala.util.parsing.combinator._
import scala.collection.mutable.HashMap

object Desk {
  def main(args: Array[String]): Unit = {
    val interpreter = new DInterpreter
    val parser = new DParser

    args foreach { file =>
      scala.io.Source.fromFile(file).getLines foreach { line =>
        println(line)
        println(interpreter.exec(parser.parse(line), new DEnvironment))
      }
    }
  }
}

class DEnvironment {
  val mem: HashMap[String, Int] = new HashMap
}

case class Assignment(name: String, num: Int)
sealed trait Expression
case class Identity(name: String) extends Expression
case class Value(num: Int) extends Expression

class DParser extends JavaTokenParsers {
  def program = ("print" ~> sums <~ "where") ~ state ^^ {case sums ~ state => (sums, state)}

  def state = repsep(assignment, ",")
  def assignment = ident ~ "=" ~ wholeNumber ^^ {case name ~ _ ~ num => (new Assignment(name, num.toInt))}

  def sums = repsep(addend, "+")
  def addend =    ident       ^^ {case name => new Identity(name)} |
  wholeNumber ^^ {case num => new Value(num.toInt)}

  def parse(s: String) = parseAll(program, s) match {
    case Success(matched, _) => matched
    case Failure(msg, _) => { println("FAILURE: " + msg)
    throw new RuntimeException }
    case Error(msg, _) => { println("ERROR: " + msg)
    throw new RuntimeException }
  }
}

class DInterpreter {
  def exec(program: (List[Expression], List[Assignment]), env: DEnvironment): Int = {
    program._2 foreach { case Assignment(name, num) => env.mem += name -> num }

    program._1.foldLeft(0)((acc:Int, expression) => expression match {
      case Identity(name) => acc + env.mem.getOrElse(name, 0)
      case Value(num) => acc + num
    })
  }
}
```
