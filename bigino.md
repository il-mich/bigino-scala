## Funzioni di parsing utili

Ripetizioni: sono una famiglia di funzioni che indica una ripetizioni di tot+ elementi eventualmente divisi da un separatore, nel pattern matching restituiscono una lista:

  - `rep(statement)`: `0+` elementi del tipo `statement`
  - `rep1(statement)`: `1+` elementi del tipo `statement`
  - `repN(n, statement)`: `n` elementi del tipo `statement`
  - `repsep(statement, separator)`: `0+` elementi del tipo `statement`, separatati dalla stringa `separator`
  - `rep1sep(statement, separator)`: `1` elementi del tipo `statement`, separatati dalla stringa `separator`
  - `opt(statement)`: indica che la presente di uno `statement` è opzionale, nel pattern matching risulta come un optional (`Some(x)` o `None`)

## Regex di JavaTokenParsers

  - `stringLiteral`: una stringa circondata da virgolette
  - `ident`: una stringa che inizia con un carattere ASCII o un underscore e seguita da un qualsiasi numero di caratteri non whitespace (aka una stringa senza virgolette).
  - `wholeNumber`: un numero intero
  - `decimalNumber`: `wholeNumber` o un un numero con la virgola
  - `floatingPointNumber`: `decimalNumber` o un numero in notazione scientifica con segno

--------------------
## Scheletro di un programma DSL



``` scala
import scala.util.parsing.combinator._

// Main
object <<LanguageName>> {
  def main(args: Array[String]): Unit = {
    val parser = new <<LanguageParser>>
    val interpreter = new <<LanguageInterpreter>>

    // For every file
    args foreach { file =>
      // Evaluate every line in a new environment
      scala.io.Source.fromFile(file).getLines foreach { line =>
        interpreter.exec(parser.parse(line), new <<LanguageEnvironment>>)
      }
    }
  }
}


// Environment
class <<LanguageEnvironment>> {
  val mem: HashMap[String, Int] = new HashMap
}


sealed trait <<LanguageMetaExpression>>
case class <<LanguageExpression>> extends <<LanguageMetaExpression>>
case class <<LanguageValue>>(value: <<ValueType>>) extends <<LanguageMetaExpression>>

case class <<LanguageOtherExpression>>(name: <<Type>>, num: <<Type>>)

// Parser
class <<LanguageParser>> extends JavaTokenParsers {
  // Remove "marker" and isolate <<LanguageElement>> as what precedes "marker2"
  def <<LanguageProgram>> = "marker" ~> <<LanguageElement>> <~ "marker2"
  def <<LanguageElement>> = repsep(<<Expression>>, ",")
  def <<Expression>> = """[a-zA-Z]+""" ^^ {case exp => new <<LanguageExpression>>}

  // Match the two elements but remove the "=" in the case using "_"
  def <<OtherExpression>> = <<LanguageExpression>> ~ "=" ~ <<LanguageValue>> ^^ {case expression ~ _ ~ value => new <<LanguageOtherExpression>>(name, value.toInt)}

  def parse(s: String) = parseAll(<<LanguageProgram>>, s) match {
    case Success(matched, _)  => matched
    case Failure(msg, _)      => {  println("FAILURE: " + msg)
                                    throw new RuntimeException }
    case Error(msg, _)        => {  println("ERROR: " + msg)
                                    throw new RuntimeException }
  }
}


// Interpreter
class <<LanguageInterpreter>> {
  def exec(program: <<ParserOutputStructure>>, env: <<LanguageEnvironment>>): Int = {
    // Program is a List, evaluate its instructions one by one
    program foreach {
      case <<LanguageExpression>> => do the expression's things
      case <<LanguageValue>>(value) => use the extracted value
    }
    // Take from program tuple, match with type Assignment, unbox name and num
    // put the in the HashMap mem
    program._2 foreach { case Assignment(name, num) => env.mem += name -> num }

    // Match with type Identity or Value, sum everything together
    program._1.foldLeft(0)((acc:Int, expression) => expression match {
      case Identity(name) => acc + env.mem.getOrElse(name, 0)
      case Value(num) => acc + num
    })
  }
}
```

---
## My implementation of Brainfuck

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

---
## My implementation of Desk

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
  def addend =  ident       ^^ {case name => new Identity(name)} |
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
