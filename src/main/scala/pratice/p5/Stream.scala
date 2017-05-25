package pratice.p5

/**
  * Created by wangj on 2017/5/24.
  */
sealed trait Stream[+A]
{
    def headOption: Option[A] = this match
    {
        case Empty => None
        case Cons(h, t) => Some(h())
    }

    // 练习5.1
    def toList: List[A] = this match
    {
        case Empty => Nil
        case Cons(h, t) => h() :: t().toList
    }

    // 练习5.1的尾递归实现
    def toListTailrec: List[A] =
    {
        def go(s: Stream[A], acc: List[A]): List[A] = s match
        {
            case Cons(h, t) => go(t(), h() :: acc)
            case _ => acc
        }

        go(this, List()).reverse
    }

    // 练习5.2 take
    def take(n: Int): Stream[A] = this match
    {
        case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
        case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
        case _ => Stream.empty
    }

    // 练习5.2 drop
    def drop(n: Int): Stream[A] = this match
    {
        case Cons(_, t) if n > 1 => t().drop(n - 1)
        case _ => this
    }

    // 练习5.3 takeWhile
    def takeWhile(f: A => Boolean): Stream[A] = this match
    {
        case Cons(h, t) if f(h()) => Stream.cons(h(), t() takeWhile f)
        case _ => Stream.empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match
    {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
    }

    // 练习5.4 forAll
    def forAll(p: A => Boolean): Boolean = foldRight(true)(p(_) && _)

    // 练习5.5
    def takeWhileUseFoldRight(f: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((h, s) => if (f(h)) Stream.cons(h, s) else Stream.empty)

    // 练习5.6
    def headOptionUseFoldRight: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

    // 练习5.7 map
    def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((h, s) => Stream.cons(f(h), s))

    // 练习5.7 filter
    def filter(f: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((h, s) => if (f(h)) Stream.cons(h, s) else s)

    // 练习5.7 append
    def append[B >: A](other: => Stream[B]): Stream[B] = foldRight(other)(Stream.cons(_, _))

    // 练习5.7 flatMap
    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty[B])(f(_).append(_))
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream
{
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    // 练习5.8
    def constant[A](a: A): Stream[A] =
    {
        lazy val ones = cons(a, ones)
        ones
    }

    // 练习5.9
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    // 练习5.10
    def fibs(): Stream[Int] =
    {
        def go(n1: Int, n2: Int): Stream[Int] =
        {
            cons(n1, go(n2, n1 + n2))
        }

        go(0, 1)
    }

    // 练习5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match
    {
        case Some((h, s)) => cons(h, unfold(s)(f))
        case None => empty
    }

    // 练习5.12 fibs
    def fibsUseUnfold(): Stream[Int] = unfold((0, 1))(pair => Some((pair._1, (pair._2, pair._1 + pair._2))))

    // 练习5.12 fibs 这个代码没看明白
    def fibsViaUnfold2(): Stream[Int] = unfold((0, 1))
    {
        case (f0, f1) => Some((f0, (f1, f0 + f1)))
    }

    // 练习5.12 from
    def fromUseUnfold(n: Int): Stream[Int] = unfold(n)(z => Some((z, z + 1)))

    // 练习5.12 constant
    def constantUseUnfold[A](a: A): Stream[A] = unfold(a)(n => Some(n, n))
}
