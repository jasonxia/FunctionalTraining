package com.rea.adt

import com.rea.adt.Expr._
import org.specs2.mutable.Specification

class ExpressionProblemExercisesSpec extends Specification {

  "eval Const" in {
    eval(Const(true)) === true
  }

  "eval And" in {
    eval(And(Const(false), Const(false))) === false
    eval(And(Const(false), Const(true))) === false
    eval(And(Const(true), Const(false))) === false
    eval(And(Const(true), Const(true))) === true
  }

  "eval Or" in {
    eval(Or(Const(false), Const(false))) === false
    eval(Or(Const(false), Const(true))) === true
    eval(Or(Const(true), Const(false))) === true
    eval(Or(Const(true), Const(true))) === true
  }

  "eval Not" in {
    eval(Not(Const(false))) === true
    eval(Not(Const(true))) === false
  }

  "eval nested" in {
    eval(Not(Or(Const(false), And(Const(true), Const(true))))) === false
  }

  "normalise" in {

    val a = Const(true)
    val b = Const(false)

    "!!a" in {
      normalise(Not(Not(a))) === a
    }

    "!!a recursively" in {
      normalise(Not(Not( Not(Not(a)) ))) === a
    }

    "!a & !b" in {
      normalise(And(Not(a), Not(b))) === Not(Or(a, b))
    }

    "!a | !b" in {
      normalise(Or(Not(a), Not(b))) === Not(And(a, b))
    }

    "!!a nested in (!a & !b)" in {
      normalise(And(Not( Not(Not(a)) ), Not(b))) === Not(Or(a, b))
    }

    "!a & (!a | !b)"  in {
      normalise(And(Not(a), Or(Not(a), Not(b)))) === And(Not(a), Not(And(a, b)))
    }

    "!(!a | !b)" in {
      normalise(Not(Or(Not(a), Not(b)))) === And(a, b)
    }
  }

  "show Const(true)" in {
    show(Const(true)) === "true"
  }

  "show Const(false)" in {
    show(Const(false)) === "false"
  }

  "show And" in {
    show(And(Const(false), Const(true))) === "false and true"
  }

  "show Or" in {
    show(Or(Const(true), Const(false))) === "true or false"
  }

  "show Not" in {
    show(Not(Const(false))) === "not false"
  }

  "show nested" in {
    show(Not(Or(Const(true), Const(false)))) === "not true or false"
  }
}


