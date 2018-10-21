class Polynomial(list: List[Int]){
  val coeffs = list.reverse
  val degree = list.size - 1

  def + (roots: Roots): Polynomial = new Polynomial(this.coeffs.toSet.union(roots.roots.toSet).toList)
  def * (roots: Roots): Polynomial = new Polynomial(this.coeffs.toSet.intersect(roots.roots.toSet).toList)

  def == (other: Polynomial): Boolean = this.toString == other.toString

  def generate(i: Int): String = {
    if (i == degree)
      coeffs.head.toString
    else if(coeffs(degree - i) != 0)
      coeffs(degree-i) + "*x^" + (degree-i).toString + " + " + generate(i + 1)
    else
      generate(i+1)
  }

  def generateMap(coeffs: List[Int], i: Int, degree: Int): List[(Int, Int)] = {
    if (i == degree)
      List((coeffs.head, 0))
    else
      (coeffs(degree-i), degree-i) :: generateMap(coeffs, i + 1, degree)
  }

  def getAddResult(first: List[Int], second: List[Int], i: Int) : List[Int] = {
    if (i == first.size - 1)
      List(first(i) + second(i))
    else
      (first(i) + second(i)) :: getAddResult(first, second, i+1)
  }

  def multiply_tuples(first: (Int, Int), second: (Int, Int)) : (Int, Int) =
    (first._1 * second._1, first._2 + second._2)

  def pack(input: List[(Int, Int)]) : List[(Int, Int)] = {
    val sortedList = input.sortBy(x => x._2).reverse
    sortedList.groupBy(_._2)
      .values
      .toList
      .map(_.reduce[(Int, Int)] { case ((a1, b1), (a2, b2)) => (a1 + a2) -> b1 })
      .sortBy(x => x._2).reverse
  }

  def multiply(first: List[(Int, Int)], second: List[(Int, Int)], i: Int, j: Int) : List[(Int, Int)] = {
    if (i == first.length)
      List()
    else if (j == second.length)
      multiply(first,second, i+1, 0)
    else
      multiply_tuples(first(i), second(j)) :: multiply(first, second, i, j + 1)
  }

  def getMulResult(first: List[Int], second: List[Int]) : List[Int] = {
    val first_map = generateMap(first, 0, first.length - 1)
    val second_map = generateMap(second, 0, second.length - 1)
    pack(multiply(first_map, second_map, 0, 0)).map(x => x._1)
  }

  def add(first: List[Int], second: List[Int]): List[Int] = {
    if (first.size < second.size)
      getAddResult(List.fill(second.size - first.size)(0) ++ first, second, 0)
    else
      getAddResult(List.fill(first.size - second.size)(0) ++ second, first, 0)
  }

  def mul(first: List[Int], second: List[Int]): List[Int] = {
    if (first.size < second.size)
      getMulResult(first ++ List.fill(second.size - first.size)(0), second)
    else if (first.size > second.size)
      getMulResult(second ++ List.fill(first.size - second.size)(0), first)
    else
      getMulResult(first, second)
  }

  def addPolynomial(other: Polynomial): Polynomial =
    new Polynomial(add(this.coeffs.reverse, other.coeffs.reverse))

  def multiplyPolynomial(other: Polynomial): Polynomial =
    new Polynomial(mul(this.coeffs, other.coeffs))

  override def toString: String = generate(0).replaceAll("\\+ \\-", "- ")
}

class Roots(list: List[Int]) {
  val roots = list

  def ! (): Polynomial = generatePolynomial(roots, 0)

  def * (other: Polynomial) : Polynomial = other * this

  def generatePolynomial(roots: List[Int], i: Int): Polynomial = {
    if (i == roots.length)
      new Polynomial(List(1))
    else
      new Polynomial(List(1, -roots(i))) multiplyPolynomial generatePolynomial(roots, i + 1)
  }

  override def toString: String = list.toString()
}

val pol1 = new Polynomial(List(1, 0, 1, 0, -3, -3))
val pol2 = new Polynomial(List(3, 0, 5, 0, -4))

val pol_equal_to_roots = new Polynomial(List(1, -2, -7, 8, 12))

val roots = new Roots(List(-1, -2, 2, 3))

val add_res = pol1.addPolynomial(pol2)
val mul_res = pol1.multiplyPolynomial(pol2)

val union_pol_roots = pol2 + roots
val intersection_pol_roots = pol2 * roots

val gen_by_roots = roots!

val equality = pol_equal_to_roots == (roots!)

val test_intersection = roots * pol2