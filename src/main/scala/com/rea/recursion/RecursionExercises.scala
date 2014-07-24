package com.rea.recursion

import scala.annotation.tailrec


// Taken from http://tmorris.net/posts/scala-exercises-for-beginners/index.html

/**
 * Ok here are the rules.
 *
 * You can't use any of the standard list functions, like map, filter, flatMap, append etc.
 *
 * You CAN and are encouraged to use the solutions from the exercises below to solve the harder
 * ones towards the end.
 *
 * Keep an eye out for repetition and similarities between your answers.
 *
 * REMEMBER: Follow the types, they almost always guide you to the solution.  If it compiles and looks a little
 * too simple, it's probably correct.  As Sherlock Holmes once said, "Each one is suggestive, together they are
 * most certainly conclusive."
 *
 * See if you can make your solution tail recursive, where possible.
 *
 */
object RecursionExercises1 {

  def plusOne(n: Int) = n + 1

  def minusOne(n: Int) = n - 1

  // Add two positive Integers together.  You are only allowed to use plusOne and minusOne above
  def add(a: Int, b: Int): Int = if (b == 0) a else add(plusOne(a), minusOne(b))

  // You are not permitted to use any list functions such as map, flatMap, ++, flatten etc
  def sum(l: List[Int]): Int = {
    @tailrec
    def sumTail(l: List[Int], acc: Int): Int = l match {
      case Nil => acc
      case h :: t => sumTail(t, add(h, acc))
    }

    sumTail(l, 0)
  }

  //Again no list functions are permitted for the following
  def length[A](x: List[A]): Int = {
    @tailrec
    def addLength(x: List[A], acc: Int): Int = x match {
      case Nil => acc
      case _ :: t => addLength(t, plusOne(acc))
    }
    addLength(x, 0)
  }

  // Do you notice anything similar between sum and length? Hmm...

  // Mapping over a list.  You are given a List of type A and a function converting an A to a B
  // and you give back a list of type B.  No list functions allowed!
  def map[A, B](x: List[A], f: A => B): List[B] = {
    @tailrec
    def mapTail(x: List[A], acc: List[B]): List[B] = x match {
      case Nil => acc
      case h :: t => mapTail(t, acc :+ f(h))
    }

    mapTail(x, List())
  }

  // Given a function from A => Boolean, return a list with only those item where the function returned true.
  def filter[A](x: List[A], f: A => Boolean): List[A] = {
    @tailrec
    def filterTail(x: List[A], acc: List[A]): List[A] = x match {
      case Nil => acc
      case h :: t => {
        if (f(h)) filterTail(t, acc :+ h ) else filterTail(t, acc)
      }
    }

    filterTail(x, List())
  }

  // This pattern should be familiar by now... psst... look at add.
  def append[A](x: List[A], y: List[A]): List[A] = x match {
    case Nil => y
    case h :: t => h :: append(t, y)
  }

  // Flatten a list of lists to a single list.  Remember you can't use list.flatten.  Can you use a previous
  // solution to solve this one?
  def flatten[A](x: List[List[A]]): List[A] ={
    @tailrec
    def flattenTail(x: List[List[A]], acc: List[A]): List[A] = x match {
      case Nil => acc
      case h :: t => flattenTail(t, append(acc, h))
    }
    flattenTail(x, List())
  }

  // Follow the types.  You've done a great job getting here. Follow the types.
  def flatMap[A, B](x: List[A], f: A => List[B]): List[B] = flatten(map(x, f))

  // Maximum of the empty list is 0
  def maximum(x: List[Int]): Int = {
    @tailrec
    def max(x: List[Int], m: Int): Int = x match {
      case Nil => m
      case h :: t => if (h > m) max(t, h) else max(t, m)
    }
    max(x, Int.MinValue)
  }

  // Reverse a list
  def reverse[A](x: List[A]): List[A] = {
    @tailrec
    def rever(x: List[A], acc: List[A]): List[A] = x match {
      case Nil => acc
      case h :: t => rever(t, h :: acc)
    }
    rever(x, List())
  }

}
