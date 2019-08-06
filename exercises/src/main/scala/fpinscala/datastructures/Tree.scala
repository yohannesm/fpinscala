package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

 def size[A](bt: Tree[A]): Int = bt match {
   case Leaf(_) => 1
   case Branch(l, r) => 1 + size(l) + size(r)
 }

  def maximum(bt: Tree[Int]): Int = bt match {
    case Leaf(i) => i
    case Branch(l, r) => Math.max(maximum(l), maximum(r))
  }

  def depth[A](bt: Tree[A]): Int = bt match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + Math.max(depth(l), depth(r))
  }

  def map[A, B](bt: Tree[A])(f: A => B): Tree[B] = bt match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](bt: Tree[A])(f: A => B)(g: (B,B) => B): B = bt match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeByFold[A](bt: Tree[A]): Int = fold(bt)(_ => 1)((b1, b2) => 1 + b1 + b2)

  def maxByFold(bt: Tree[Int]): Int = fold(bt)(identity(_))((b1, b2) => Math.max(b1, b2))

  def depthByFold(bt: Tree[Int]): Int = fold(bt)(_ => 0)((b1, b2) => 1 + Math.max(b1, b2))

  def identityTree[A](bt: Tree[A]): Tree[A] = fold(bt)(Leaf(_): Tree[A])(Branch(_, _))

  /*
  if you do
  def identityTree[A](bt: Tree[A]): Tree[A] = fold(bt)(Leaf(_))(Branch(_, _))

  you'll get an error
    type mismatch;
    found   : fpinscala.datastructures.Branch[B]
    required: fpinscala.datastructures.Leaf[B]

  This error is an unfortunate consequence of Scala using subtyping to encode algebraic data types. Without the
    annotation, the result type of the fold gets inferred as `Leaf[B]` and it is then expected that the second argument
    to `fold` will return `Leaf[B]`, which it doesn't (it returns `Branch[B]`). Really, we'd prefer Scala to
  infer `Tree[B]` as the result type in both cases. When working with algebraic data types in Scala, it's somewhat
  common to define helper functions that simply call the corresponding data constructors but give the less specific
  result type:

  def leaf[A](a: A): Tree[A] = Leaf(a)
  def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)

  Or you need to specify the specific type of ` Leaf(_): Tree[A] ` like in the identityTree function above
  */

  def leaf[A](a: A): Tree[A] = Leaf(a)
  def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)

  def mapByFold[A, B](bt: Tree[A])(f: A => B): Tree[B] =
    fold(bt)(a => Leaf(f(a)) : Tree[B])(Branch(_, _))

  def mapByFold2[A, B](bt: Tree[A])(f: A => B): Tree[B] =
    fold(bt)(a => leaf(f(a)))(branch(_, _))
}