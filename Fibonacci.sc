class Fibonacci(a: Int) {

  val lst = binary_representation(a)
  val num = a

  def find_largest_fib_num(a: Int, i: Int) : Int = {
    if (fib(i) > a) fib(i-1)
    else find_largest_fib_num(a, i+1)
  }

  def fib(n: Int): Int = n match {
    case 0 | 1 => n
    case _ => fib(n-1) + fib(n-2)
  }

  def get_fib_position(n: Int, num: Int) : Int = {
    if (fib(num) == n) num
    else get_fib_position(n, num + 1)
  }

  def is_fib_num(n: Int, i: Int): Boolean = {
    if(fib(i) == n) true
    else if (fib(i) > n) false
    else is_fib_num(n, i+1)
  }

  def binary_representation(n: Int): List[Int] = {
    if (n == 0) List(0)
    else if(is_fib_num(n, 0)) generate_fib_represent(n)
    else generateBinary(generateList(a, flag = false), 0).reverse
  }

  def generate_fib_represent(n: Int): List[Int] = {
    val pos = get_fib_position(n, 0)
    1 :: List.fill(pos-2)(0)
  }

  def generateList(a: Int, flag: Boolean): List[Int] = {
    val largest = find_largest_fib_num(a, 0)
    val remainder = a - largest
    if (flag) List()
    else {
      if (is_fib_num(largest, 0)) {
        val fib_pos = get_fib_position(largest, 0)
        val pos = fib_pos - 2
        if (remainder == 0)  {
          if (fib_pos != 1)
            pos :: generateList(remainder, flag = true)
          else fib_pos - 1 :: generateList(remainder, flag = true)
        }
        else if (fib_pos != 1) pos :: generateList(remainder, flag = false)
        else fib_pos - 1 :: generateList(remainder, flag = false)
      }
      else
        generateList(remainder, flag = false)
    }
  }

  def generateBinary(lst: List[Int], i: Int) : List[Int] = {
    if (i == lst.head) List(1)
    else {
      if (lst.contains(i))
        1 :: generateBinary(lst, i + 1)
      else
        0 :: generateBinary(lst, i + 1)
    }
  }

  def get_union(first: List[Int], second: List[Int], i: Int): List[Int] = {
    if (i == first.size) List()
    else {
      if (first(i) == 1 && second(i) == 1)
        1 :: get_union(first, second, i+1)
      else
        0 :: get_union(first, second, i+1)
    }
  }

  def get_num_by_list(input: List[Int], i: Int, temp: Int) : Int = {
    if(i == input.size) temp
    else
      if  (input(i) == 0) get_num_by_list(input, i+1, temp)
      else get_num_by_list(input, i+1, temp + fib((input.size - i)+1))
  }

  def sum_list(first: List[Int], second: List[Int], i: Int): List[Int] = {
    if (i == first.size) List()
    else first(i) + second(i) :: sum_list(first, second, i+1)
  }

  def first_stage(input: List[Int], start: Int, end: Int, size: Int): List[Int] = {
    if (end == size) input
    else
    {
      val input_str = input mkString ""
      val temp_str = input.slice(start, end) mkString ""
      val res = temp_str match {
        case "020"  => "100" + (input(end) + 1).toString
        case "030"  => "110" + (input(end) + 1).toString
        case "021"  => "110" + input(end).toString
        case "012"  => "101" + input(end).toString
        case _      => input_str.slice(start, end+1)
      }
      val result = input_str.slice(0, start) + res + input_str.slice(start + 4, input_str.length)
      first_stage(result.toCharArray.toList.map(x => (x - '0').toInt), start + 1, end + 1, size)
    }
  }

  def step_one(input: String, i: Int, size: Int): String = {
    if (i+3 == size) input
    else {
      if (input.slice(i, i + 3) == "110")
        step_one(input.slice(0, i) + "001" + input.slice(i + 3, input.length), i + 1, size)
      else step_one(input, i + 1, size)
    }
  }

  def step_two(input: String, i: Int, size: Int): String = {
    if (i + 3 == size) input
    else {
      if (input.slice(i, i + 3) == "011")
        step_two(input.slice(0, i) + "100" + input.slice(i + 3, input.length), i + 1, size)
      else step_two(input, i + 1, size)
    }
  }

  def second_stage(input: List[Int]): List[Int] = {
    val input_str = input mkString ""
    val res = input_str
      .replace("0120","1010").replace("030","111")
      .replace("003","100").replace("020","101")
      .replace("003","100").replace("012","101")
      .replace("021","110").replace("02","10")
      .replace("03","11")
      .reverse
    val temp = (step_one(res, 0, res.length) + "0").reverse
    step_two(temp, 0, temp.length).toCharArray.toList.map(x => (x - '0').toInt)
  }

  def add(first: List[Int], second: List[Int], i: Int): List[Int] = {
    val add_result = List(0) ++ sum_list(first, second, 0)
    val first_stage_result = first_stage(add_result, 0, 3, add_result.size)
    second_stage(first_stage_result)
  }

  def +(other: Fibonacci): Fibonacci = {
    if (this.lst.size < other.lst.size)
      new Fibonacci(get_num_by_list(add(List.fill(other.lst.size - this.lst.size)(0) ++ this.lst, other.lst, 0), 0, 0))
    else if (other.lst.size < this.lst.size)
      new Fibonacci(get_num_by_list(add(this.lst, List.fill(this.lst.size - other.lst.size)(0) ++ other.lst, 0), 0, 0))
    else
      new Fibonacci(get_num_by_list(add(this.lst, other.lst, 0), 0, 0))
  }

  def %(other: Fibonacci): Fibonacci = {
    if (this.lst.size < other.lst.size)
      new Fibonacci(get_num_by_list(get_union(List.fill(other.lst.size - this.lst.size)(0) ++ this.lst, other.lst, 0), 0, 0))
    else if (other.lst.size < this.lst.size)
      new Fibonacci(get_num_by_list(get_union(this.lst,  List.fill(this.lst.size - other.lst.size)(0) ++ other.lst, 0), 0, 0))
    else
      new Fibonacci(get_num_by_list(get_union(this.lst, other.lst, 0), 0, 0))
  }

  def toInteger: BigInt = BigInt(this.toString)

  override def toString: String = lst mkString ""
}


for (i <- 0 to 20) {
  val fib = new Fibonacci(i)
  println(fib, fib.get_num_by_list(fib.lst, 0, 0))
}

val r = scala.util.Random

for (i <- 0 to 9) {
  val number_first = r.nextInt(20)
  val number_second = r.nextInt(20)
  val first = new Fibonacci(number_first)
  val second = new Fibonacci(number_second)
  if (number_first + number_second == first.get_num_by_list((first+second).lst, 0, 0))
    println("True", number_first, number_second)
}
val first = new Fibonacci(7)
val second = new Fibonacci(9)

first % second
first + second

first.toInteger
second.toInteger