package scala.js

import java.io.{PrintStream,File,FileInputStream,FileOutputStream,ByteArrayOutputStream}
import org.scalatest._


class FileDiffSuite2(prefix: String) { this: Suite =>
  
  def testWithOutFile(name: String)(test: java.io.PrintWriter => Unit) {
    withOutFile(prefix+name)(test(new java.io.PrintWriter(System.out)))
    assertFileEqualsCheck(prefix+name)
  }
  
  def withOutFile(name: String)(func: => Unit) {
    val file = new File(name)
    file.getParentFile.mkdirs()
    withOutput(new PrintStream(new FileOutputStream(file)))(func)
  }
  def captureOutput(func: => Unit): String = {
    val bstream = new ByteArrayOutputStream
    withOutput(new PrintStream(bstream))(func)
    bstream.toString
  }
  def withOutput(out: PrintStream)(func: => Unit) {
    val oldStdOut = System.out
    val oldStdErr = System.err
    try {
      System.setOut(out)
      System.setErr(out)
      Console.withOut(out)(Console.withErr(out)(func))
    } finally {
      out.flush()
      out.close()
      System.setOut(oldStdOut)
      System.setErr(oldStdErr)
    }
  }
  
  def readFile(name: String): String = {
    val buf = new Array[Byte](new File(name).length().toInt)
    val fis = new FileInputStream(name)
    fis.read(buf)
    fis.close()
    new String(buf)
  }
  def assertFileEqualsCheck(name: String) {
    expectResult(readFile(name+".check")){readFile(name)}
    new File(name) delete ()
  }
}
