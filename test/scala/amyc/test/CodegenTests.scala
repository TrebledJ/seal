// package amyc.test

// import amyc.parsing._
// import amyc.analyzer.{NameAnalyzer, TypeChecker}
// import amyc.codegen._
// import amyc.wasm.Module
// import amyc.utils._
// import scala.sys.process._
// import scala.collection.JavaConverters._
// import java.io.ByteArrayInputStream

// class CodegenTests extends ExecutionTests {

//   object CodePrinterExecutor extends Pipeline[Module, Unit] {
//     def run(ctx: Context)(m: Module) = {
//       CodePrinter.run(ctx)(m)
//       val fileName = s"${m.name}.js"

//       // Consume all standard input!
//       val input = Console.in.lines.iterator().asScala.toList.mkString("\n")
//       val inputS = new ByteArrayInputStream(input.getBytes("UTF-8"))

//       val exitCode = s"node wasmout/$fileName" #< inputS ! ProcessLogger(Console.out.println, Console.err.println)
//       if (exitCode != 0)
//         throw AmycFatalError("Nonzero code returned from nodejs")
//     }
//   }

//   val pipeline =
//     Lexer andThen
//     Parser andThen
//     NameAnalyzer andThen
//     TypeChecker andThen
//     CodeGen andThen
//     CodePrinterExecutor
// }
