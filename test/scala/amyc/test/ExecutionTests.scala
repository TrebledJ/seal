package amyc.test

import org.junit.Test

abstract class ExecutionTests extends TestSuite {

  val baseDir = "interpreter"

  val outputExt = "txt"

  @Test def testEmptyObject = shouldOutput("EmptyObject")
  @Test def testHello = shouldOutput(List("Std", "Hello"), "Hello")
  @Test def testMath = shouldOutput(List("Std", "Math"), "Math")
  @Test def testVariableDecl = shouldOutput("VariableDecl")
  @Test def testConditionTrue = shouldOutput(List("Std", "ConditionTrue"), "ConditionTrue")
  @Test def testConditionFalse = shouldOutput(List("Std", "ConditionFalse"), "ConditionFalse")
  @Test def testBasic = shouldOutput(List("Std", "Basic"), "Basic")
  @Test def testEqualsLiteral = shouldOutput(List("Std", "EqualsLiteral"), "EqualsLiteral")
  @Test def testEqualsADT = shouldOutput(List("Std", "List", "Option", "EqualsADT"), "EqualsADT")
  @Test def testTrickyEquals = shouldOutput(List("Std", "List", "Option", "TrickyEquals"), "TrickyEquals")
  @Test def testFunction = shouldOutput(List("Std", "Function"), "Function")
  @Test def testIntMatch = shouldOutput(List("Std", "IntMatch"), "IntMatch")
  @Test def testIdMatch = shouldOutput(List("Std", "IdMatch"), "IdMatch")
  @Test def testCaseClassMatch = shouldOutput(List("Std", "List", "Option", "CaseClassMatch"), "CaseClassMatch")
  @Test def testCaseClassMatch2 = shouldOutput(List("Std", "List", "Option", "CaseClassMatch2"), "CaseClassMatch2")
  @Test def testAOC2020Day1 = shouldOutput(List("Std", "List", "Option", "AOC2020D1"), "AOC2020D1")
  @Test def testMoreList = shouldOutput(List("Std", "List", "Option", "MoreList", "Ext"), "MoreList")
  @Test def testExtTuple0 = shouldOutput(List("Std", "List", "Option", "ExtTuple0"), "ExtTuple0")
  @Test def testExtTuple1 = shouldOutput(List("Std", "List", "Option", "ExtTuple1"), "ExtTuple1")
  @Test def testExtTuple2 = shouldOutput(List("Std", "List", "Option", "ExtTuple2"), "ExtTuple2")
  @Test def testExtTuple3 = shouldOutput(List("Std", "List", "Option", "ExtTuple3"), "ExtTuple3")
  @Test def testExtTuple20 = shouldOutput(List("Std", "ExtTuple20"), "ExtTuple20")
  @Test def testExtTuple21 = shouldOutput(List("Std", "ExtTuple21"), "ExtTuple21")
  @Test def testExtTuple22 = shouldOutput(List("Std", "ExtTuple22"), "ExtTuple22")
  @Test def testExtTuple23 = shouldOutput(List("Std", "ExtTuple23"), "ExtTuple23")
  @Test def testExtTuple24 = shouldOutput(List("Std", "ExtTuple24"), "ExtTuple24")
  @Test def testExtTuple25 = shouldOutput(List("Std", "ExtTuple25"), "ExtTuple25")
  @Test def testExtTuple26 = shouldOutput(List("Std", "ExtTuple26"), "ExtTuple26")
  @Test def testExtTuplePattern0 = shouldOutput(List("Std", "List", "Option", "ExtTuplePattern0"), "ExtTuplePattern0")
  @Test def testExtTuplePattern1 = shouldOutput(List("Std", "List", "Option", "ExtTuplePattern1"), "ExtTuplePattern1")
  @Test def testExtTuplePattern2 = shouldOutput(List("Std", "List", "Option", "ExtTuplePattern2"), "ExtTuplePattern2")
  @Test def testExtTuplePattern3 = shouldOutput(List("Std", "List", "Option", "ExtTuplePattern3"), "ExtTuplePattern3")
  @Test def testExtType4 = shouldOutput(List("Std", "ExtType4"), "ExtType4")
  @Test def testExtType41 = shouldOutput(List("Std", "ExtType4.1"), "ExtType4.1")
  @Test def testExtLambda1 = shouldOutput(List("Std", "ExtLambda1"), "ExtLambda1")
  @Test def testExtLambda2 = shouldOutput(List("Std", "ExtLambda2"), "ExtLambda2")
  @Test def testExtLambda3 = shouldOutput(List("Std", "ExtLambda3"), "ExtLambda3")
  @Test def testExtLambda4 = shouldOutput(List("Std", "ExtLambda4"), "ExtLambda4")
  @Test def testExtLambda5 = shouldOutput(List("Std", "ExtLambda5"), "ExtLambda5")
  @Test def testExtLambda51 = shouldOutput(List("Std", "ExtLambda5.1"), "ExtLambda5.1")
  @Test def testExtLambda6 = shouldOutput(List("Std", "ExtLambda6"), "ExtLambda6")
  @Test def testExtLambda7 = shouldOutput(List("Std", "ExtLambda7"), "ExtLambda7")
  @Test def testExtLambda8 = shouldOutput(List("Std", "ExtLambda8"), "ExtLambda8")
  @Test def testExtLambda10 = shouldOutput(List("Std", "ExtLambda10"), "ExtLambda10")
  @Test def testExtLambda11 = shouldOutput(List("Std", "ExtLambda11"), "ExtLambda11")
  @Test def testExtLambda12 = shouldOutput(List("Std", "ExtLambda12"), "ExtLambda12")
  @Test def testHigherOrderFunctions = shouldOutput(List("Std", "List", "Option", "HigherOrderFunctions"), "HigherOrderFunctions")
  
  @Test def testMinimalError = shouldFail("MinimalError")
  @Test def testGhostVariable = shouldFail("GhostVariable")
  @Test def testDuplicate = shouldFail("Duplicate")
  @Test def testDuplicate2 = shouldFail("Duplicate2")
  @Test def testDivisionByZero = shouldFail("DivisionByZero")
  @Test def testMatchString = shouldFail("MatchString")

}
