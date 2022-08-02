# SEAL: Some Extensions for the Amy Language

Interested? Read the [report](report/report.pdf)!

This project was part of a course on compiler design.

Everything here is meant for education purposes only and is meant to be used as a reference. **Please do NOT copy or plagiarise anything here besides the past papers.**

## Extensions

See examples of these extensions in [extension-examples](./extension-examples). I would like to draw particular attention to [SealSim](https://github.com/TrebledJ/seal/blob/master/extension-examples/SealSim.scala), the epitome of SEAL.

### [Built-in Tuples](https://github.com/TrebledJ/seal/blob/master/extension-examples/Tuples.scala)
SEAL supports tuples and typechecking tuples.

```scala
(1, 2);  // Construct a tuple.
val t: (Int, Int) = (3, 4); // Tuple types.
t(0);    // Access a tuple -- returns 3.
t match {
  case (x, y) => ...  // Pattern match on a tuple.
};
```

### [Higher Order Functions](https://github.com/TrebledJ/seal/blob/master/extension-examples/HigherOrderFunctions.scala)
SEAL also supports higher order functions, including function types, lambdas, and currying. These days, most languages support passing functions around as first class citizens to enable more abstract logic and avoid duplication of code.

```scala
def operate(f: Int => Int): Int => Int = { // Function types.
  \(x: Int) -> f(x)   // Lambda.
}
// `operate` is a higher order function since it takes in a function as parameter.

val inc: Int => Int = operate(\(x: Int) -> x + 1);
inc(0); // 1
inc(1); // 2

operate(\(x: Int) -> x * 5)(6); // Currying -- returns 30.
```

### Extended Standard Library

New functions for convenience working with IO, Strings, randomness, etc. These can all be found in the [library](./library).

* `File.open(filename: String): Reader` -- Opens a file and returns a reader object. Errors if file can't be opened.
* Read functions -- Functions for reading lines and tokens.
  * `File.readLine(file: Reader): String`
  * `File.readString(file: Reader): String`
  * `File.readInt(file: Reader): Int`
  * `File.readBoolean(file: Reader): Boolean`
* `File.isEOF(file: Reader): Boolean` -- Checks if a file has hit EOF.
* `String.equals(a: String, b: String): Boolean` -- For comparing strings.
* `Random.int(n: Int): Int` -- Generates an integer between [0, n).
* `Random.range(min: Int, max: Int): Int` -- Generates an integer between [min, max].
* `Random.oneIn(n: Int): Boolean` -- Picks a number between [0, n) and returns true if you're lucky.
