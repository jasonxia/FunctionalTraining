package com.rea.adt


sealed trait Expr
case class Const(value: Boolean) extends Expr
case class And(a: Expr, b: Expr) extends Expr
case class Or(a: Expr, b: Expr) extends Expr
case class Not(expr: Expr) extends Expr

/**
 * Use pattern matching and recursion.  No vars, no loops, no overriding.
 */
object Expr {
  
  /**
   * Evaluate the expression.
   */
  def eval(expr: Expr): Boolean = expr match {
    case Const(a)  => a
    case And(a, b) => eval(a) && eval(b)
    case Or(a, b)  => eval(a) || eval(b)
    case Not(a)    => !eval(a)
  }
  
  /**
   * Normalise the expression, such that: 
   * !!a     ==> a
   * !a & !b ==> !(a | b)
   * !a | !b ==> !(a & b)
   */
  def normalise(expr: Expr): Expr = expr match {
    case Const(_) => expr
    case Not(Const(_)) => expr
    case Not(Not(a)) => normalise(a)
    case Not(a) => normalise(Not(normalise(a)))
    case And(Not(a), Not(b)) => Not(Or(normalise(a), normalise(b)))
    case And(a, b) => And(normalise(a), normalise(b))
    case Or(Not(a), Not(b)) => Not(And(normalise(a), normalise(b)))
    case Or(a, b) => Or(normalise(a), normalise(b))
  }

  /**
   * Show, using English lower-case words "and", "or", "not", "true", "false"
   */
  def show(expr: Expr): String = expr match {
    case Const(a)  => a.toString
    case And(a, b) => show(a) + " and " + show(b)
    case Or(a, b)  => show(a) + " or " + show(b)
    case Not(a)    => "not " + show(a)
  }
  
}
