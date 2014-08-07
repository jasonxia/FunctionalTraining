package com.rea.typesafety

import org.specs2.mutable.Specification
import OptionalExercises3._

class OptionalExercises3Spec extends Specification {
  "flatMap" should {
    "operate on a Just" in {
      flatMap(Just(1), {a:Int => Just(a + 1)}) should be equalTo (Just(2))
    }
  }

  "flatMap" should {
    "operate on a Just" in {
      flatMap(Just(1), {a:Int => Just(a + 1)}) should be equalTo Just(2)
    }

    "operate on Nothing" in {
      flatMap(Nothing, {a:Int => Just(a + 1)}) should be equalTo Nothing
    }
  }

  "map" should {
    "operate on a Just" in {
      OptionalExercises3.map(Just(1), {a:Int => a + 1}) should be equalTo Just(2)
    }

    "operate on Nothing" in {
      OptionalExercises3.map(Nothing, {a:Int => a + 1}) should be equalTo Nothing
    }
  }

  "map2" should {
    val sum = map2((a: Int, b: Int) => a + b) _

    "operate on two Nothings" in {
      sum(Nothing, Nothing) should be equalTo Nothing
    }

    "operate on a Nothing and a Just" in {
      sum(Nothing, Just(1)) should be equalTo Nothing
      sum(Just(1), Nothing) should be equalTo Nothing
    }

    "operate on two Justs" in {
      sum(Just(1), Just(2)) should be equalTo Just(3)
    }
  }

  "fold" should {
    "operate on a Just" in {
      fold(Just(1), 10, {a:Int => a + 1}) should be equalTo 2
    }

    "operate on Nothing" in {
      fold(Nothing, 10, {a:Int => a + 1}) should be equalTo 10
    }
  }

  "orElse" should {
    "operate on a Just" in {
      orElse(Just(1), Just(2)) should be equalTo Just(1)
    }

    "operate on Nothing" in {
      orElse(Nothing, Just(2)) should be equalTo Just(2)
    }
  }

  "sequence" should {
    "operate on a empty list" in {
      sequence(Nil) should be equalTo Just(Nil)
    }

    "operate on a list with a Nothing" in {
      sequence(List(Just(1), Nothing, Just(2))) should be equalTo Nothing
    }

    "operate on a list with only Justs" in {
      sequence(List(Just(1), Just(2), Just(3))) should be equalTo Just(List(1, 2, 3))
    }
  }

  "collect" should {
    "operate on a empty list" in {
      collect(Nil) should be equalTo Just(Nil)
    }

    "operate on a list Nothing" in {
      collect(List(Nothing, Nothing, Nothing)) should be equalTo Just(Nil)
    }

    "operate on a list with a Nothing" in {
      collect(List(Just(1), Nothing, Just(2))) should be equalTo Just(List(1, 2))
    }
  }

}
