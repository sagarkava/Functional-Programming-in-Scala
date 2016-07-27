package constraints

import cafesat.api.FormulaBuilder._
import cafesat.api.Formulas._
import cafesat.api.Solver._

import scala.annotation.tailrec

/**
  * This object contains utility functions for encoding
  * some arithmetic constraints as boolean ones
  */
object Arithmetic {

  /**
    * Transforms a positive integer in binary form into its integer representation.
    * The `head` element of the input list contains the most
    * significant bit (the list is in big-endian form).
    */
  def binary2int(n: List[Boolean]): Int =
    n.reverse.zipWithIndex.foldLeft(0)((acc: Int, c: (Boolean, Int)) =>
      if (c._1) (acc + math.pow(2, c._2)).toInt
      else acc
    )


  /**
    * Encodes a positive integer number into base 2.
    * The `head` element of the resulting list contains the most significant
    * bit. This function should not return unnecessary leading zeros.
    */
  def int2binary(n: Int): List[Boolean] = {
    @tailrec
    def helper(r: Int, acc: List[Boolean]): List[Boolean] =
      if (r == 0) acc
      else helper(r / 2, (r % 2 == 1) :: acc)

    if (n == 0) List(false)
    else helper(n, List())
  }

  def normalize(n1: List[Formula], n2: List[Formula]): (List[Formula], List[Formula]) = {
    def extend(n: Int, ls: List[Formula]): List[Formula] =
      (for {e <- 0 until n} yield False).toList ::: ls

    if (n1.size == n2.size) (n1, n2)
    else if (n1.size < n2.size) (extend(n2.size - n1.size, n1), n2)
    else (n1, extend(n1.size - n2.size, n2))
  }

  /**
    * This function takes two arguments, both representing positive
    * integers encoded in binary as lists of propositional formulas
    * (true for 1, false for 0). It returns
    * a formula that represents a boolean circuit that constraints
    * `n1` to be less than or equal to `n2`
    */
  def lessEquals(n1: List[Formula], n2: List[Formula]): Formula = {
    val adjusted = normalize(n1, n2)

    def helper(ls1: List[Formula], ls2: List[Formula], result: Formula, x: Formula): Formula = (ls1, ls2) match {
      case (Nil, Nil) => result
      case (a :: tailA, b :: tailB) => helper(tailA, tailB, (x && a && !b) || result, x && (a iff b))
    }
    !helper(adjusted._1, adjusted._2, False, True)
  }

  /**
    * A full adder is a circuit that takes 3 one bit numbers, and returns the
    * result encoded over two bits: (cOut, s)
    */
  def fullAdder(a: Formula, b: Formula, cIn: Formula): (Formula, Formula) =
    ((a && b) || (cIn && (a || b)), a xor b xor cIn)

  /**
    * This function takes two arguments, both representing positive integers
    * encoded as lists of propositional variables. It returns a pair.
    *
    * The first element of the pair is a `List[Formula]`, and it represents
    * the resulting binary number.
    * The second element is a set of intermediate constraints that are created
    * along the way.
    *
    */
  def adder(n1: List[Formula], n2: List[Formula]): (List[Formula], Set[Formula]) = {
    // make the two list of the same length
    val adjusted: (List[Formula], List[Formula]) = normalize(n1, n2)
    // recursive method computing the sum and creating the constraints set
    def add(l1: List[Formula], l2: List[Formula], constraints: Set[Formula], res: List[Formula], carry: Formula): (List[Formula], Set[Formula]) =
      (l1, l2) match {
        case (bit1 :: tail1, bit2 :: tail2) => {
          val tmp: (Formula, Formula) = fullAdder(bit1, bit2, carry)
          val rest: PropVar = propVar()
          val result: PropVar = propVar()
          add(tail1, tail2, constraints + (rest iff tmp._1) + (result iff tmp._2), result :: res, rest)
        }
        case (Nil, Nil) => (res, constraints)
      }
    // computing the result
    val result = add((False :: adjusted._1).reverse, (False :: adjusted._2).reverse, Set(), List(), False)
    // making sure there no leading zeros in result
    if (result._1.head == False) (result._1.tail, result._2)
    else result
  }

  /**
    * A helper function that creates a less-equals formula
    * taking an integer and a formula as parameters
    */
  def lessEqualsConst(cst: Int, n: List[Formula]): Formula = {
    lessEquals(int2binary(cst), n)
  }

  /**
    * A helper function that creates a less-equals formula
    * taking a formula and an integer as parameters
    */
  def lessEqualsConst(n: List[Formula], cst: Int): Formula = {
    lessEquals(n, int2binary(cst))
  }


}
