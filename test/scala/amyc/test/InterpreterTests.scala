package amyc
package test

import amyc.parsing._
import amyc.analyzer.{NameAnalyzer, TypeChecker}
import interpreter.Interpreter

class InterpreterTests extends ExecutionTests {
  val pipeline = Lexer andThen Parser andThen NameAnalyzer andThen TypeChecker andThen Interpreter
}
