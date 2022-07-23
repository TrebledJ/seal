package amyc.test

import amyc.parsing._
import org.junit.Test

class LexerTests extends TestSuite {
  val pipeline = Lexer andThen DisplayTokens

  val baseDir = "lexer"

  val outputExt = "txt"

  @Test def testKeywords = shouldOutput("Keywords")
  @Test def testWhitespace = shouldOutput("Whitespace")
  @Test def testCommentClosedTwice = shouldOutput("CommentClosedTwice")
  @Test def testHello = shouldOutput("Hello")
  @Test def testPrinting = shouldOutput("Printing")
  @Test def testGoodInt = shouldOutput("GoodInt")
  @Test def testTrickyComment = shouldOutput("TrickyComment")
  @Test def testIdentifiers = shouldOutput("Identifiers")
  @Test def testAOC2020D1 = shouldOutput("AOC2020D1")

  @Test def testSingleAmp = shouldFail("SingleAmp")
  @Test def testUnclosedComment3 = shouldFail("UnclosedComment3")
  @Test def testBadInt = shouldFail("BadInt")
  @Test def testBadInt2 = shouldFail("BadInt2")
  @Test def testBadInt3 = shouldFail("BadInt3")
  @Test def testMultilineString = shouldFail("MultilineString")

}
