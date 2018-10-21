class Pos private ( val prog : String , val offs : Int , val line : Int , val col : Int ) {
  def this (prog : String) = this (prog , 0, 1, 1)
  def ch = if (offs == prog.length) -1 else prog(offs).toInt
  def inc = ch match {
    case '\n' => new Pos(prog,offs +1,line +1,1)
    case -1 => this
    case _ => new Pos(prog,offs +1,line,col +1)
  }
  override def toString = "(" + line + "," + col + ")"
}

object DomainTags extends Enumeration {
  type Tag = Value
  val WHITESPACE , IDENT , NUMBER , END_OF_PROGRAM, KEYWORD, STRING, COMMENT, UNKNOWN = Value
}

import DomainTags._

class Scanner {
  def scan (start : Pos, errors: List[String]): (Tag,Pos,List[String]) =
    sys.error("syntax error at " + start)
}


class Token ( val start : Pos , scanner : Scanner, val errors: List[String]) {

  def this(start : Pos, scanner : Scanner) = this(start,scanner,List())

  val (tag, follow,list_error) = now()

  def now(): (Tag,Pos,List[String]) = {
    start.ch match {
      case -1 => (END_OF_PROGRAM, start, errors)
      case _ => scanner.scan(start, errors)
    }
  }

  def image = start.prog.substring(start.offs,follow.offs)
  def next = new Token(follow,scanner,list_error)
}

trait Whitespaces extends Scanner {
  private def missWhitespace (pos : Pos): Pos = pos.ch match {
    case ' ' | '\t' | '\n'  => missWhitespace (pos.inc)
    case _                  => pos
  }

  override def scan (start : Pos, errors: List[String]) = {
    val follow = missWhitespace ( start )
    if (start != follow) (WHITESPACE,follow,errors)
    else super.scan(start,errors)
  }
}

trait Idents extends Scanner {

  private def is_palindrome(str: String): Boolean = str == str.reverse

  private def scanIdent(pos: Pos, flag: Boolean): (Pos, Boolean) = {
    (pos, flag) match {
      case (a, false) if Character.isLetter(a.ch) || a.ch == '-'  => scanIdent(pos.inc, flag = false)
      case (a, b)                                                 => (a, true)
    }
  }

  private def is_ident_case(start: Pos): Boolean = {
    if (start.ch  == '-') {
      val next_ch = start.inc.ch
      if (!Character.isLetter(next_ch) && next_ch != '-') false
      else true
    }
    else if (Character.isLetter(start.ch)) true
    else false
  }

  override def scan(start: Pos, errors: List[String]) = {
    if (is_ident_case(start)) {
      val follow = scanIdent(start, flag = false)
      if (start != follow._1) {
        val image = start.prog.substring(start.offs, follow._1.offs)
        if (image.equals("dod") || image.equals("if")) (KEYWORD, follow._1, errors)
        else if (is_palindrome(image)) (IDENT, follow._1, errors)
        else {
          val add_error = "not a palindrom at " + start :: errors
          (UNKNOWN, follow._1, add_error)
        }
      }
      else super.scan(start, errors)
    }
    else super.scan(start, errors)
  }
}

trait Numbers extends Scanner {
  private def scanNumber(pos : Pos,flag: Boolean) : (Pos,Boolean) = {
    (pos,flag) match {
      case (c,d) if Character.isDigit(c.ch)     => scanNumber(pos.inc, flag = false)
      case (c,d)                                => (c,true)
    }
  }

  override def scan(start: Pos,errors:List[String]) = {
      if (start.ch == '-') {
        if (!Character.isDigit(start.inc.ch)) super.scan(start, errors)
        else {
          val follow = scanNumber(start.inc, flag = false)
          if (follow._2 && follow._1 != start.inc) (NUMBER, follow._1, errors)
          else super.scan(start, errors)
        }
      } else {
          val follow = scanNumber(start, flag = false)
          if (follow._1 != start) (NUMBER, follow._1, errors)
          else super.scan(start, errors)
      }
  }
}

trait Comments extends Scanner {
  private def scanComment(pos: Pos, flag: Boolean) : (Pos, Boolean) = {
    (pos,flag) match {
      case (c,d) if c.ch == '\n'  => (c, true)
      case (c,d) if c.ch == -1    => (c, true)
      case (c,d)                  => scanComment(c.inc, d)
    }
  }
  override def scan(start: Pos, errors: List[String]) = {
    val checkComment = (start.ch == '-') && (start.inc.ch == '-')
    if (checkComment) {
      val follow = scanComment(start.inc.inc, flag = false)
      if (follow._2) (COMMENT, follow._1, errors)
      else {
        val add_error = "error in comment at " + start :: errors
        (COMMENT, follow._1, add_error)
      }
    }
    else super.scan(start, errors)
  }
}

trait Unknown extends Scanner {
  override def scan(start: Pos, errors: List[String]) = {
    val add_error = "Unknown token at " + start + ": " + start.prog(start.offs) :: errors
    (UNKNOWN, start.inc, add_error)
  }
}

var t = new Token(
  new Pos(" 2135678 -1 _234 -a-bb-bb-a- done -123 a1 a-a --sda--a -0  --s\n hahah opapo if dod \n    \t -234 --asdasdasdadad\n aaa bsss -saas- if dod"),
  new Scanner
    with Unknown
    with Idents
    with Comments
    with Numbers
    with Whitespaces
)
while (t.tag != END_OF_PROGRAM) {
  if (t.tag != UNKNOWN) {
    println(t.tag.toString + " " + t.start + "-" + t.follow + ": " + t.image)
  }
  t = t.next
}
if (t.list_error.nonEmpty) {
  println("Errors:")
  t.list_error.reverse.foreach(println)
}