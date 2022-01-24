``` scala
import scala.util.parsing.combinator._

object Brainfuck {
  def main(args: Array[String]): Unit = {
    val parser = new BFParser
    val interpreter = new BFInterpreter

    val lines = scala.io.Source.fromFile(args.head).getLines.mkString

    interpreter.exec(parser.parse(lines), new BFEnvironment)
  }
}

sealed trait Statement
case class Loop(expressions: List[Statement]) extends Statement
case class IncrementPointer() extends Statement
case class DecrementPointer() extends Statement
case class IncrementData() extends Statement
case class DecrementData() extends Statement
case class PrintData() extends Statement
case class ReadData() extends Statement

class BFParser extends JavaTokenParsers {
  override protected val whiteSpace = """[^><\]\[+.,-]+""".r

  def program = statementList
  def statementList:Parser[List[Statement]] = rep(statement)
  def statement = command | loop
  def loop = "[" ~> statementList <~ "]" ^^ {case l => new Loop(l)}
  def command = """[><+.,-]""".r ^^ {
    case ">" => new IncrementPointer
    case "<" => new DecrementPointer
    case "+" => new IncrementData
    case "-" => new DecrementData
    case "." => new PrintData
    case "," => new ReadData
  }

  def parse(s: String) = parseAll(program, s) match {
    case Success(matched, _) => matched
    case Failure(msg, _) => { println("FAILURE: " + msg)
                              throw new RuntimeException }
    case Error(msg, _) => { println("ERROR: " + msg)
                            throw new RuntimeException }
  }
}

class BFInterpreter {
  def exec(prog: List[Statement], env: BFEnvironment): Unit = {
    prog.foreach {
      case IncrementPointer() => env.incPtr
      case DecrementPointer() => env.decPtr
      case IncrementData()    => env.incData
      case DecrementData()    => env.decData
      case PrintData()        => env.printPtr
      case ReadData()         => env.readPtr
      case Loop(cmds)         => while (env.mem(env.pointer) != 0) { exec(cmds, env) }
    }
  }
}

class BFEnvironment {
  var pointer = 0
  val mem = Array.fill[Byte](30000)(0)

  def incPtr = pointer += 1

  def decPtr = pointer -= 1

  def incData = mem(pointer) = (mem(pointer) + 1).toByte

  def decData = mem(pointer) = (mem(pointer) - 1).toByte

  def printPtr = print(mem(pointer).toChar)

  def readPtr = mem(pointer) = Console.in.read.toByte
}
```
