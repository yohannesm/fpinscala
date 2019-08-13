package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def head[A](l: List[A]): A = l match {
    case Nil        => throw new RuntimeException("List is empty")
    case Cons(h, _) => h
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def appendList[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, tail) => tail
    case _             => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil              => throw new NotImplementedError()
    case Cons(head, tail) => Cons(h, tail)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else
      l match {
        case Nil              => Nil
        case Cons(head, tail) => drop(tail, n - 1)
      }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(head, tail) =>
        if (f(head)) dropWhile(tail, f)
        else l
    }
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil              => Nil
    case Cons(_, Nil)     => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((l, z) => 1 + z)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil              => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  def sumFoldLeft(l: List[Int]) = foldLeft(l, 0)(_ + _)
  def productFoldLeft(l: List[Int]) = foldLeft(l, 1)(_ * _)
  def lengthFoldLeft[A](l: List[A]) = foldLeft(l, 0)((z, l) => 1 + z)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((z, head) => Cons(head, z))

  def foldRightByFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldLeftByFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, delayFunc) => b => delayFunc(f(b, a)))(z)

  def append[A](l: List[A], r: List[A]): List[A] =
    foldRightByFoldLeft(l, r)(Cons(_, _))

  def concat[A](ll: List[List[A]]): List[A] =
    foldRightByFoldLeft(ll, Nil: List[A])(append(_, _))

  def add1(l: List[Int]): List[Int] =
    foldRightByFoldLeft(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  def lstDoubleToLstString(l: List[Double]): List[String] =
    foldRightByFoldLeft(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRightByFoldLeft(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRightByFoldLeft(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRightByFoldLeft(as, Nil: List[B])((h, t) => append(f(h), t))

  def filterByFlatmap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  //any better impl? These aren't stack safe
  //might need to implement iterators?
  def zipInts(la: List[Int], lb: List[Int]): List[Int] = (la, lb) match {
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(ha, ta), Cons(hb, tb)) => Cons(ha + hb, zipInts(ta, tb))
  }

  def zipWith[A](la: List[A], lb: List[A])(f: (A, A) => A): List[A] =
    (la, lb) match {
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb)(f))
    }

  def zipWith_r1[A](la: List[A], lb: List[A])(f: (A, A) => A): List[A] = {
    @annotation.tailrec
    def builder[A](la: List[A], lb: List[A], rlist: List[A])(
        f: (A, A) => A): List[A] =
      if (la == Nil || lb == Nil) rlist
      else
        builder(tail(la),
                tail(lb),
                Cons(f(List.head(la), List.head(lb)), rlist))(f)

    reverse(builder(la, lb, Nil)(f))
  }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil)                              => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _                                     => false
  }
  @annotation.tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil                     => sub == Nil
    case _ if startsWith(l, sub) => true
    case Cons(_, t)              => hasSubsequence(t, sub)
  }

  def myHasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def go[A](l: List[A], currentSub: List[A], origSub: List[A]): Boolean =
      (l, currentSub) match {
        case (_, Nil) => true
        //partial match in progress, passing tail and tailsub until Nil
        case (Cons(h, t), Cons(hs, ts)) if h == hs => go(t, ts, origSub)
        //current match fails, passing in rest of tail + original sub as sublist to retry
        case (Cons(h, t), Cons(hs, _)) if h != hs && hs == List.head(origSub) =>
          go(t, origSub, origSub)
        //this case happens in the middle of partial match and it fails.
        //recurse with the original list again, and retry
        case (Cons(h, _), Cons(hs, _)) if h != hs && hs != List.head(origSub) =>
          go(l, origSub, origSub)
        case _ => false
      }
    go(l, sub, sub)
  }

}
