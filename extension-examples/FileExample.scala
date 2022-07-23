object FileExample {
  val file: File.Reader = File.open("examples/Hello.scala");
  val s: String = File.readLine(file);
  Std.printString(s)
}
