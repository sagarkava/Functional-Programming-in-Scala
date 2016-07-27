package interpreter

object Main extends App {
  import java.io.{BufferedReader, InputStreamReader}
  val in = new BufferedReader(new InputStreamReader(System.in))
  // TODO: Insert code for the REPL

  while (true) {
    val line = in.readLine()
    Lisp.evaluate(line)
  }

}

object LispCode {
  // TODO: implement the function `reverse` in Lisp.
  // From a list (a, b, c, d) it should compute (d, c, b, a)
  // Write it as a String, and test it in your REPL
  val reverse = """
  def (reverse L acc)
    (if (null? L) acc (reverse (cdr L) (cons (car L) acc)))
  """

  // TODO: implement the function `differences` in Lisp.
  // From a list (a, b, c, d ...) it should compute (a, b-a, c-b, d-c ...)
  // You might find useful to define an inner loop def
  val differences = """
  def (differences L)
    (def (iter L acc)
      (if (null? (cdr L)) (cons (car L) acc) (iter (cdr L) (cons (- (car L) (car (cdr L))) acc))) (if (null? L) nil (iter (reverse L nil) nil)))
  """

  val rebuildList = """
  def (rebuildList L)
    (def (iter L acc) (if (null? L) acc (iter (cdr L) (cons (+ (car acc) (car L)) acc)))
    (if (null? L) nil (cdr (reverse (iter L (cons 0 nil)) nil))))
  """

  val withDifferences: String => String =
    (code: String) => "(" + reverse + " (" + differences + " (" + rebuildList + " " + code + ")))"
}
