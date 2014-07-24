package com.rea.higherorder

import scala.annotation.tailrec

/*
 * DO NOT ATTEMPT these exercises until you've completed the recursion ones.
 */

object FoldingExercises {

  /**
   *
   * foldLeft will reduce a list of A's down to a B. It takes an initial value of type B
   * and a list of A's.  It also takes a function which takes the accumulated value of type B
   * and the next value in the list (of type A) and returns a value which will be feed back into
   * the accumulator of the next call.
   *
   * As the name suggests it processes the list from left to right.
   *
   * Have a close look at your implementations from the RecursionExercises.  Which parts could you
   * pull out to a function to make them all common?  Your implementation will be very close to
   * foldLeft.
   *
   * Good luck!
   *
   */
  @tailrec
  def foldLeft[A, B](initialValue: B, list: List[A])(f: (B, A) => B): B = list match {
    case Nil => initialValue
    case h :: t => foldLeft(f(initialValue, h), t)(f)
  }

  /**
   * foldRight is the same as foldLeft, except it processes the list from right to left.
   */
  def foldRight[A,B](initialValue:B, list: List[A])(f: (A,B) => B):B = list.reverse match {
    case Nil => initialValue
    case h :: t => foldRight(f(h, initialValue), t.reverse)(f)
  }

  /**
   * Remember these, from our recursion exercises?  They can all be implemented with either
   * foldLeft or foldRight.
   */

  def sum(l: List[Int]): Int = foldLeft(0, l)(_ + _)

  def length[A](x: List[A]): Int = foldLeft(0, x)((acc, _) => acc + 1)

  def map[A, B](x: List[A])(f: A => B): List[B] = foldRight(List[B](), x)(f(_) :: _)

  def filter[A](x: List[A], f: A => Boolean): List[A] = foldRight(List[A](), x)((e, acc) => if (f(e)) e :: acc else acc )

  def append[A](x: List[A], y: List[A]): List[A] = foldRight(y, x)(_ :: _)

  def flatten[A](x: List[List[A]]): List[A] = foldLeft(List[A](), x)((acc, l) => acc ::: l)

  def flatMap[A, B](x: List[A], f: A => List[B]): List[B] = flatten(map(x)(f))

  // Maximum of the empty list is 0
  def maximum(x: List[Int]): Int = foldLeft(0, x)((max, e) => if(e > max) e else max)

  def reverse[A](x: List[A]): List[A] = foldLeft(List[A](), x)((acc, e) => e :: acc)

}
