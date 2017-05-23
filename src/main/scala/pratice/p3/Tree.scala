package pratice.p3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree
{
    // 练习3.25
    def size[A](t: Tree[A]): Int = t match
    {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + size(l) + size(r)
    }

    // 练习3.26
    def maximum(t: Tree[Int]): Int = t match
    {
        case Leaf(v) => v
        case Branch(l, r) => maximum(l) max maximum(r)
    }

    // 练习3.27
    def depth[A](t: Tree[A]): Int = t match
    {
        case Leaf(_) => 0
        case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

    // 练习3.28
    def map[A, B](t: Tree[A], f: A => B): Tree[B] = t match
    {
        case Leaf(v) => Leaf(f(v))
        case Branch(l, r) => Branch(map(l, f), map(r, f))
    }

    // 练习3.29
    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match
    {
        case Leaf(a) => f(a)
        case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def sizeFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

    def maximumFold(t: Tree[Int]): Int = fold(t)(v => v)(_ max _)

    def depthFold[A](t: Tree[A]): Int = fold(t)(_ => 0)(1 + _ max _)

    def mapFold[A, B](t: Tree[A], f: A => B): Tree[B] = fold(t)(v => Leaf(f(v)): Tree[B])((l, r) => Branch(l, r))
}
