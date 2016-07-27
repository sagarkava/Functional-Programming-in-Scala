def differences (ls: List[Int]): List[Int] = ls match {
  case List(_) => Nil
  case a :: b :: tail => b - a :: differences(tail)
}

val a = List(1,2,3,4,5)
differences(a)