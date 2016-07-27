import cafesat.api.Formulas.Formula

def fullAdder(a: Boolean, b: Boolean, cIn: Boolean): (Boolean, Boolean) =
  ((a && b) || (cIn && (a || b)), a ^ b ^ cIn)

def adder(l1: List[Boolean], l2: List[Boolean])= {
  def add(l1: List[Boolean], l2: List[Boolean], res: List[Boolean], carry: Boolean): List[Boolean] = (l1, l2) match {
    case (bit1 :: tail1, bit2 :: tail2) => {
      val intermediate = fullAdder(bit1, bit2, carry)
      add(tail1, tail2, intermediate._2 :: res, intermediate._1)
    }
    case (Nil, Nil) => res
  }
  add((false :: l1).reverse, (false :: l2).reverse, List(), false)
}

adder(List(true), List(true))
