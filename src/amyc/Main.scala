package amyc

import utils._
import ast._
import parsing._
import analyzer._
import codegen._
import interpreter._

import java.io.File

object Main extends MainHelpers {
  private def parseArgs(args: Array[String]): Context = {
    Context(new Reporter, args.toList)
  }

  def main(args: Array[String]): Unit = {
    val ctx = parseArgs(args)

    val parsePipeline = 
      Lexer andThen
      Parser andThen
      treePrinterN("Trees after parsing")

    val nameAnalyserPipeline =
      Lexer andThen
      Parser andThen
      NameAnalyzer andThen
      treePrinterS("Trees after analysing")
    
    val typeCheckerPipeline =
      Lexer andThen
      Parser andThen
      NameAnalyzer andThen
      TypeChecker andThen
      treePrinterS("Trees after type checking")

    val fullPipeline =
      Lexer andThen
      Parser andThen
      NameAnalyzer andThen
      TypeChecker andThen
      Interpreter
      // CodeGen andThen
      // CodePrinter

    // val pipeline = parsePipeline
    // val pipeline = nameAnalyserPipeline
    // val pipeline = typeCheckerPipeline
    val pipeline = fullPipeline

    val files = ctx.files.map(new File(_))

    try {
      if (files.isEmpty) {
        ctx.reporter.fatal("No input files")
      }
      files.find(!_.exists()).foreach { f =>
        ctx.reporter.fatal(s"File not found: ${f.getName}")
      }
      pipeline.run(ctx)(files)
      ctx.reporter.terminateIfErrors()
    } catch {
      case AmycFatalError(_) =>
        sys.exit(1)
    }
  }
}

trait MainHelpers {
  import SymbolicTreeModule.{Program => SP}
  import NominalTreeModule.{Program => NP}

  def treePrinterS(title: String): Pipeline[(SP, SymbolTable), Unit] = {
    new Pipeline[(SP, SymbolTable), Unit] {
      def run(ctx: Context)(v: (SP, SymbolTable)) = {
        println(title)
        println(SymbolicPrinter(v._1)(true))
      }
    }
  }

  def treePrinterN(title: String): Pipeline[NP, Unit] = {
    new Pipeline[NP, Unit] {
      def run(ctx: Context)(v: NP) = {
        println(title)
        println(NominalPrinter(v))
      }
    }
  }
}
