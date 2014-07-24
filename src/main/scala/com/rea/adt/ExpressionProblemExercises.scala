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
  def normalise(expr: Expr): Expr = ???
  
  /**
   * Show, using English lower-case words "and", "or", "not", "true", "false"
   */
  def show(expr: Expr): String = expr match {
    case Const(a)  => a.toString
    case And(a, b) => eval(a) + " and " + eval(b)
    case Or(a, b)  => eval(a) + " or " + eval(b)
    case Not(a)    => " not " + eval(a)
  }
  
}
