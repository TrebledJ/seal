package amyc.test

import amyc.parsing._
import amyc.ast.{Identifier, SymbolicPrinter}
import amyc.ast.SymbolicTreeModule.Program
import amyc.analyzer.{NameAnalyzer, TypeChecker}
import amyc.utils._
import org.junit.Test

class TyperTests extends TestSuite {
  // We need a unit pipeline
  private def unit[A]: Pipeline[A, Unit] = {
    new Pipeline[A, Unit] {
      def run(ctx: Context)(v: A) = ()
    }
  }

  val pipeline = Lexer andThen Parser andThen NameAnalyzer andThen TypeChecker andThen unit

  val baseDir = "typer"

  val outputExt = "" // No output files for typechecking

  @Test def testArithmetic = shouldPass("Arithmetic")
  @Test def testMatch = shouldPass("Match")
  @Test def testSimpleClass = shouldPass("SimpleClass")
  @Test def testQualifiedClass = shouldPass(List("Std", "Option", "List", "QualifiedClass"), "QualifiedClass")
  @Test def testMatchClass = shouldPass(List("Std", "Option", "List", "MatchClass"), "MatchClass")
  @Test def testCall = shouldPass("Call")
  @Test def testAOC2020D1 = shouldPass(List("Std", "Option", "List", "AOC2020D1"), "AOC2020D1")
  @Test def testExt = shouldPass("Ext")
  @Test def testFactorial = shouldPass(List("Std", "Factorial"), "Factorial")
  @Test def testHanoi = shouldPass(List("Std", "Hanoi"), "Hanoi")
  @Test def testHello = shouldPass(List("Std", "Hello"), "Hello")
  @Test def testHelloInt = shouldPass(List("Std", "HelloInt"), "HelloInt")
  @Test def testPrinting = shouldPass(List("Std", "Printing"), "Printing")
  @Test def testTestLists = shouldPass(List("Std", "Option", "List", "TestLists"), "TestLists")
  @Test def testTypeTest = shouldPass("TypeTest")

  @Test def testExtType0 = shouldPass("ExtType0")
  @Test def testExtType1 = shouldPass("ExtType1")
  @Test def testExtType2 = shouldPass("ExtType2")
  @Test def testExtType3 = shouldPass("ExtType3")
  @Test def testExtType4 = shouldPass(List("Std", "ExtType4"), "ExtType4")
  @Test def testExtType10 = shouldPass("ExtType10")
  @Test def testExtTuple0 = shouldPass(List("Std", "ExtTuple0"), "ExtTuple0")
  @Test def testExtTuple1 = shouldPass(List("Std", "ExtTuple1"), "ExtTuple1")
  @Test def testExtTuple2 = shouldPass(List("Std", "ExtTuple2"), "ExtTuple2")
  @Test def testExtTuple3 = shouldPass(List("Std", "ExtTuple3"), "ExtTuple3")
  @Test def testExtTuple20 = shouldPass(List("Std", "ExtTuple20"), "ExtTuple20")
  @Test def testExtTuple21 = shouldPass(List("Std", "ExtTuple21"), "ExtTuple21")
  @Test def testExtTuple22 = shouldPass(List("Std", "ExtTuple22"), "ExtTuple22")
  @Test def testExtTuple23 = shouldPass(List("Std", "ExtTuple23"), "ExtTuple23")
  @Test def testExtTuple24 = shouldPass(List("Std", "ExtTuple24"), "ExtTuple24")
  @Test def testExtTuple25 = shouldPass(List("Std", "ExtTuple25"), "ExtTuple25")
  @Test def testExtTuple26 = shouldPass(List("Std", "ExtTuple26"), "ExtTuple26")
  @Test def testExtTuple27 = shouldPass(List("Std", "ExtTuple27"), "ExtTuple27")
  @Test def testExtTuplePattern0 = shouldPass(List("Std", "ExtTuplePattern0"), "ExtTuplePattern0")
  @Test def testExtTuplePattern1 = shouldPass(List("Std", "List", "Option", "ExtTuplePattern1"), "ExtTuplePattern1")
  @Test def testExtTuplePattern2 = shouldPass(List("Std", "ExtTuplePattern2"), "ExtTuplePattern2")
  @Test def testExtTuplePattern3 = shouldPass(List("Std", "List", "Option", "ExtTuplePattern3"), "ExtTuplePattern3")
  @Test def testExtCall0 = shouldPass(List("Std", "ExtCall0"), "ExtCall0")
  @Test def testExtCall1 = shouldPass(List("Std", "ExtCall1"), "ExtCall1")
  @Test def testExtCall2 = shouldPass(List("Std", "ExtCall2"), "ExtCall2")
  @Test def testExtCall3 = shouldPass(List("Std", "ExtCall3"), "ExtCall3")
  @Test def testExtCall4 = shouldPass(List("Std", "ExtCall4"), "ExtCall4")
  @Test def testExtCall5 = shouldPass(List("Std", "ExtCall5"), "ExtCall5")
  @Test def testExtCall6 = shouldPass(List("Std", "ExtCall6"), "ExtCall6")
  @Test def testExtCall7 = shouldPass(List("Std", "ExtCall7"), "ExtCall7")
  @Test def testExtCall10 = shouldPass(List("Std", "ExtCall10"), "ExtCall10")
  @Test def testExtCall11 = shouldPass(List("Std", "ExtCall11"), "ExtCall11")
  @Test def testExtLambda0 = shouldPass(List("Std", "ExtLambda0"), "ExtLambda0")
  @Test def testExtLambda1 = shouldPass(List("Std", "ExtLambda1"), "ExtLambda1")
  @Test def testExtLambda2 = shouldPass(List("Std", "ExtLambda2"), "ExtLambda2")
  @Test def testExtLambda3 = shouldPass(List("Std", "ExtLambda3"), "ExtLambda3")
  @Test def testExtLambda4 = shouldPass(List("Std", "ExtLambda4"), "ExtLambda4")
  @Test def testExtLambda5 = shouldPass(List("Std", "ExtLambda5"), "ExtLambda5")
  @Test def testExtLambda6 = shouldPass(List("Std", "ExtLambda6"), "ExtLambda6")
  @Test def testExtLambda7 = shouldPass(List("Std", "ExtLambda7"), "ExtLambda7")
  @Test def testExtLambda8 = shouldPass(List("Std", "ExtLambda8"), "ExtLambda8")
  @Test def testExtLambda10 = shouldPass(List("Std", "ExtLambda10"), "ExtLambda10")
  @Test def testExtLambda11 = shouldPass(List("Std", "ExtLambda11"), "ExtLambda11")
  @Test def testExtLambda01 = shouldPass(List("Std", "ExtLambda0.1"), "ExtLambda0.1")
  @Test def testExtLambda1$1 = shouldPass(List("Std", "ExtLambda1.1"), "ExtLambda1.1")
  @Test def testExtLambda21 = shouldPass(List("Std", "ExtLambda2.1"), "ExtLambda2.1")
  @Test def testExtLambda31 = shouldPass(List("Std", "ExtLambda3.1"), "ExtLambda3.1")
  @Test def testExtLambda41 = shouldPass(List("Std", "ExtLambda4.1"), "ExtLambda4.1")
  @Test def testExtLambda51 = shouldPass(List("Std", "ExtLambda5.1"), "ExtLambda5.1")
  @Test def testExtLambda52 = shouldPass(List("Std", "ExtLambda5.2"), "ExtLambda5.2")
  @Test def testExtLambda53 = shouldPass(List("Std", "ExtLambda5.3"), "ExtLambda5.3")
  @Test def testExtLambda61 = shouldPass(List("Std", "ExtLambda6.1"), "ExtLambda6.1")
  @Test def testExtLambda71 = shouldPass(List("Std", "ExtLambda7.1"), "ExtLambda7.1")
  @Test def testExtLambda81 = shouldPass(List("Std", "ExtLambda8.1"), "ExtLambda8.1")
  @Test def testExtLambda101 = shouldPass(List("Std", "ExtLambda10.1"), "ExtLambda10.1")
  @Test def testExtLambda111 = shouldPass(List("Std", "ExtLambda11.1"), "ExtLambda11.1")
  @Test def testExtLambda12 = shouldPass(List("Std", "ExtLambda12"), "ExtLambda12")
  @Test def testExtLambda13 = shouldPass(List("Std", "ExtLambda13"), "ExtLambda13")

  @Test def testArithError1 = shouldFail("ArithError1")
  @Test def testArithError2 = shouldFail("ArithError2")
  @Test def testArithError3 = shouldFail("ArithError3")
  @Test def testArithError4 = shouldFail("ArithError4")
  @Test def testArithError5 = shouldFail("ArithError5")
  @Test def testLetError1 = shouldFail("LetError1")
  @Test def testLetError2 = shouldFail("LetError2")
  @Test def testOperatorError1 = shouldFail("OperatorError1")
  @Test def testOperatorError2 = shouldFail("OperatorError2")
  @Test def testOperatorError3 = shouldFail("OperatorError3")
  @Test def testSeqError3 = shouldFail("SeqError3")
  @Test def testMatchError1 = shouldFail("MatchError1")
  @Test def testMatchError2 = shouldFail("MatchError2")
  @Test def testMatchError3 = shouldFail("MatchError3")
  @Test def testCallError1 = shouldFail("CallError1")
  @Test def testCallError2 = shouldFail("CallError2")
  @Test def testCallError3 = shouldFail("CallError3")
  @Test def testCallError4 = shouldFail(List("Std", "CallError4"), "CallError4")
  @Test def testCallError5 = shouldFail(List("Std", "CallError5"), "CallError5")
  @Test def testCallError6 = shouldFail(List("Std", "List", "Option", "CallError6"), "CallError6")
  @Test def testCallError7 = shouldFail(List("Std", "List", "Option", "CallError7"), "CallError7")
  @Test def testCallError8 = shouldFail(List("Std", "List", "Option", "CallError8"), "CallError8")
  @Test def testCallError9 = shouldFail(List("Std", "List", "Option", "CallError9"), "CallError9")

  @Test def testArgumentNumberFunction = shouldFail("ArgumentNumberFunction")

  @Test def testExtTypeError0 = shouldFail(List("Std", "ExtTypeError0"), "ExtTypeError0")
  @Test def testExtTypeError1 = shouldFail(List("Std", "ExtTypeError1"), "ExtTypeError1")
  @Test def testExtTypeError2 = shouldFail(List("Std", "ExtTypeError2"), "ExtTypeError2")
  @Test def testExtTypeError41 = shouldFail(List("Std", "ExtTypeError4.1"), "ExtTypeError4.1")
  @Test def testExtTypeError42 = shouldFail(List("Std", "ExtTypeError4.2"), "ExtTypeError4.2")
  @Test def testExtTupleError1 = shouldFail(List("Std", "ExtTupleError1"), "ExtTupleError1")
  @Test def testExtTupleError2 = shouldFail(List("Std", "ExtTupleError2"), "ExtTupleError2")
  @Test def testExtTupleError3 = shouldFail(List("Std", "ExtTupleError3"), "ExtTupleError3")
  @Test def testExtTupleError4 = shouldFail(List("Std", "ExtTupleError4"), "ExtTupleError4")
  @Test def testExtTupleError5 = shouldFail(List("Std", "ExtTupleError5"), "ExtTupleError5")
  @Test def testExtTupleError6 = shouldFail(List("Std", "ExtTupleError6"), "ExtTupleError6")
  @Test def testExtTupleError7 = shouldFail(List("Std", "ExtTupleError7"), "ExtTupleError7")
  @Test def testExtTupleError8 = shouldFail(List("Std", "ExtTupleError8"), "ExtTupleError8")
  @Test def testExtTupleError9 = shouldFail(List("Std", "ExtTupleError9"), "ExtTupleError9")
  @Test def testExtTupleError10 = shouldFail(List("Std", "ExtTupleError10"), "ExtTupleError10")
  @Test def testExtTupleError20 = shouldFail(List("Std", "ExtTupleError20"), "ExtTupleError20")
  @Test def testExtTupleError21 = shouldFail(List("Std", "ExtTupleError21"), "ExtTupleError21")
  @Test def testExtTupleError21$1 = shouldFail(List("Std", "ExtTupleError21.1"), "ExtTupleError21.1")
  @Test def testExtTupleError22 = shouldFail(List("Std", "ExtTupleError22"), "ExtTupleError22")
  @Test def testExtTupleError22$1 = shouldFail(List("Std", "ExtTupleError22.1"), "ExtTupleError22.1")
  @Test def testExtTupleError23 = shouldFail(List("Std", "ExtTupleError23"), "ExtTupleError23")
  @Test def testExtTuplePatternError0 = shouldFail(List("Std", "List", "Option", "ExtTuplePatternError0"), "ExtTuplePatternError0")
  @Test def testExtTuplePatternError1 = shouldFail(List("Std", "List", "Option", "ExtTuplePatternError1"), "ExtTuplePatternError1")
  @Test def testExtTuplePatternError2 = shouldFail(List("Std", "List", "Option", "ExtTuplePatternError2"), "ExtTuplePatternError2")
  @Test def testExtTuplePatternError3 = shouldFail(List("Std", "List", "Option", "ExtTuplePatternError3"), "ExtTuplePatternError3")
  @Test def testExtTuplePatternError4 = shouldFail(List("Std", "List", "Option", "ExtTuplePatternError4"), "ExtTuplePatternError4")
  @Test def testExtCallError0 = shouldFail(List("Std", "ExtCallError0"), "ExtCallError0")
  @Test def testExtCallError1 = shouldFail(List("Std", "ExtCallError1"), "ExtCallError1")
  @Test def testExtCallError2 = shouldFail(List("Std", "ExtCallError2"), "ExtCallError2")
  @Test def testExtLambdaError0 = shouldFail(List("Std", "ExtLambdaError0"), "ExtLambdaError0")
  @Test def testExtLambdaError1 = shouldFail(List("Std", "ExtLambdaError1"), "ExtLambdaError1")
  @Test def testExtLambdaError2 = shouldFail(List("Std", "ExtLambdaError2"), "ExtLambdaError2")
  @Test def testExtLambdaError3 = shouldFail(List("Std", "ExtLambdaError3"), "ExtLambdaError3")
  @Test def testExtLambdaError4 = shouldFail(List("Std", "ExtLambdaError4"), "ExtLambdaError4")
  @Test def testExtLambdaError5 = shouldFail(List("Std", "ExtLambdaError5"), "ExtLambdaError5")
  @Test def testExtLambdaError6 = shouldFail(List("Std", "ExtLambdaError6"), "ExtLambdaError6")
  @Test def testExtLambdaError13 = shouldFail(List("Std", "ExtLambdaError13"), "ExtLambdaError13")
}
