class Polynom[T](val list: List[T]) {
  def degree = list.size - 1

  def diff(i : Int) (implicit N: Numeric[T]) : List[T] = {
    if (i == list.size) List()
    else {
      val size = list.size - 1
      val current_value = N.times(list(i), N.fromInt(degree - i))
      current_value :: diff(i + 1)
    }
  }

  def padd_left(lst: List[T]): List[T] = lst.dropRight(1)

  def differentiate() (implicit N: Numeric[T]): Polynom[T] = new Polynom[T](padd_left(diff(0)))

  override def toString: String = list.toString()
}

val test = new Polynom[Int](List(1,2,3))
test.differentiate()
val testing = new Polynom[String](List("11", "1", "2"))
