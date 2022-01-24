## Funzioni di parsing utili

Ripetizioni: sono una famiglia di funzioni che indica una ripetizioni di tot+ elementi eventualmente divisi da un separatore, nel pattern matching restituiscono una lista:

  - `rep(statement)`: $0+$ elementi del tipo `statement`
  - `rep1(statement)`: $1+$ elementi del tipo `statement`
  - `repN(n, statement)`: $n$ elementi del tipo `statement`
  - `repsep(statement, separator)`: $0+$ elementi del tipo `statement`, separatati dalla stringa `separator`
  - `rep1sep(statement, separator)`: $1$ elementi del tipo `statement`, separatati dalla stringa `separator`
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
    case Success(matched, _)   => matched
    case Failure(msg, _)              => {   println("FAILURE: " + msg)
                                                                throw new RuntimeException }
    case Error(msg, _)                  => {  println("ERROR: " + msg)
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
