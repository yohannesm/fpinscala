package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = toListTailRec

  def toListRecursive: List[A] = this match {
    case Cons(h, t) => h() :: t().toListRecursive
    case Empty      => List[A]()
  }

  def toListTailRec: List[A] = {
    def go(s: Stream[A], l: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: l)
      case _          => l
    }
    go(this, List[A]()).reverse
  }

  def toListOptimized: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) =>
        f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def functionalFind(p: A => Boolean): Option[A] =
    filter(p).headOption

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => this
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileFR(p: A => Boolean): Stream[A] =
    foldRight(empty[A])(
      (h, t) =>
        if (p(h)) cons(h, t)
        else empty[A])

  def headOption: Option[A] = this match {
    case Cons(h, _) => Some(h())
    case _          => None
  }

  def headOptionFR: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    this.foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def mapWithUnfold[B](f: A => B): Stream[B] =
    unfold(this) { x =>
      x match {
        case Cons(h, t) => Some(f(h()), t())
        case Empty      => None
      }
    }

  def takeWithUnfold(n: Int): Stream[A] =
    unfold((this, n)) { x =>
      x match {
        case (Cons(h, _), 1)          => Some((h(), (empty, 0)))
        case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
        case _                        => None
      }
    }

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] =
    unfold(this) { x =>
      x match {
        case Cons(h, t) if p(h()) => Some(h(), t())
        case _                    => None
      }
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = ???

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  //simple solution for constant[A](a: A): Stream[A]
  def simpleConstant[A](a: A): Stream[A] = Stream.cons(a, simpleConstant(a))

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = cons(a, tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(n: Int): Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] = cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None         => empty[A]
  }

  def fibsWithUnfold(n: Int): Stream[Int] = {
    unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }
  }

  def fromWithUnfold(n: Int): Stream[Int] =
    unfold(n)(s => Some(s, s + 1))

  def constantWithUnfold[A](a: A): Stream[A] = {
    unfold(a)(a => Some(a, a))
  }

  def onesWithUnfold: Stream[Int] = constantWithUnfold(1)

}
