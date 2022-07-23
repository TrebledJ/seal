/** This module contains support for File I/O in Amy,
  * including stub implementations for some built-in functions.
  */
object File {
    abstract class Reader

    def open(filename: String): Reader = {
        error("") // Stub implementation.
    }

    def isEOF(file: Reader): Boolean = {
        error("") // Stub implementation.
    }

    def readLine(file: Reader): String = {
        error("") // Stub implementation.
    }

    def readInt(file: Reader): Int = {
        error("") // Stub implementation.
    }

    def readString(file: Reader): String = { 
        error("") // Stub implementation.
    }

    def readBoolean(file: Reader): Boolean = { 
        error("") // Stub implementation.
    }
}