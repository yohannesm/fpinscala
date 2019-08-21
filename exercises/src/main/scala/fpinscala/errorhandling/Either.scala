package fpinscala.errorhandling

import scala.{
  Either => _,
  Left => _,
  Option => _,
  Right => _,
  _
} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e)  => Left(e)
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e)  => Left(e)
    case Right(v) => f(v)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_)  => b
    case Right(v) => Right(v)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)

  def map2ByFlatmaps[EE >: E, B, C](b: Either[EE, B])(
      f: (A, B) => C): Either[EE, C] =
    this.flatMap(a => b.map(bb => f(a, bb)))
}
case class Left[+E](get: E) extends Either[E, Nothing]
case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E, List[B]]](Right(Nil): Either[E, List[B]])(
      (a, eitherLB) => f(a).map2(eitherLB)(_ :: _))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  /*
There are a number of variations on `Option` and `Either`. If we want to accumulate multiple errors, a simple approach is a new data type that lets us keep a list of errors in the data constructor that represents failures:

trait Partial[+A,+B]
case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
case class Success[+B](get: B) extends Partial[Nothing,B]

There is a type very similar to this called `Validation` in the Scalaz library. You can implement `map`, `map2`, `sequence`, and so on for this type in such a way that errors are accumulated when possible (`flatMap` is unable to accumulate errors--can you see why?). This idea can even be generalized further--we don't need to accumulate failing values into a list; we can accumulate values using any user-supplied binary function.

It's also possible to use `Either[List[E],_]` directly to accumulate errors, using different implementations of helper functions like `map2` and `sequence`.
 */

}

sealed trait Partial[+A, +B]
case class Errors[+A](get: Seq[A]) extends Partial[A, Nothing]
case class Success[+B](get: B) extends Partial[Nothing, B]

sealed trait EitherL[+E, +A] {
  def map[B](f: A => B): EitherL[E, B] = this match {
    case l @ LLeft(_) => l
    case LRight(get)  => LRight(f(get))
  }

  def map2[EE >: E, B, C](b: EitherL[EE, B])(f: (A, B) => C): EitherL[EE, C] =
    this match {
      case LLeft(la) =>
        b match {
          case LLeft(lb) => LLeft(la ++ lb)
          case LRight(_) => LLeft(la)
        }
      case LRight(a) =>
        b match {
          case lleft @ LLeft(_) => lleft
          case LRight(b)        => LRight(f(a, b))
        }
    }

  //flatmap is impossible because of the function type it's being passed on (need to be
  // f: A => EitherL[EE, B], which can't accumulate errors because we will lose the list in LLeft
  // , but if we pass it a function that goes from EitherL to EitherL might be possible

  def bizarreFlatMap[EE >: E, B](
      f: EitherL[EE, A] => EitherL[EE, B]): EitherL[EE, B] =
    f(this)

}
case class LLeft[+E](get: Seq[E]) extends EitherL[E, Nothing]
case class LRight[+A](get: A) extends EitherL[Nothing, A]

object EitherL {

  def traverse[E, A, B](es: List[A])(
      f: A => EitherL[E, B]): EitherL[E, List[B]] =
    es.foldRight[EitherL[E, List[B]]](LRight(Nil): EitherL[E, List[B]])(
      (a, eitherLB) => f(a).map2(eitherLB)(_ :: _))

  def sequence[E, A](es: List[EitherL[E, A]]): EitherL[E, List[A]] =
    traverse(es)(identity)

}
