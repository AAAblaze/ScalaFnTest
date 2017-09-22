package pratice.p3


// sealed关键字意味着，这个特质的所有实现都必须定义在这个文件里
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List
{
    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

    // 练习3.2 实现tail函数，删除一个List的第一个元素
    def tail[A](l: List[A]): List[A] = l match
    {
        case Nil => sys.error("tail of empty list")
        case Cons(_, t) => t
    }

    // 练习3.3 实现函数setHead，用一个不同的值替代列表中的第一个元素
    def setHead[A](l: List[A], v: A): List[A] = l match
    {
        case Nil => sys.error("setHead of empty list")
        case Cons(_, t) => Cons(v, t)
    }

    // 练习3.4 把tail泛化为drop，用于从列表中删除前n个元素
    def drop[A](l: List[A], n: Int): List[A] =
    {
        if (n <= 0) l
        else
        {
            l match
            {
                case Nil => Nil
                case Cons(_, t) => drop(t, n - 1)
            }
        }
    }

    // 练习3.5 删除列表中前缀全部符合判定的元素
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match
    {
        case Cons(h, t) if f(h) => dropWhile(t, f)
        case _ => l
    }

    // 30页的样例代码
    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match
    {
        case Nil => a2
        case Cons(h, t) => Cons(h, append(t, a2))
    }

    // 练习3.6 返回除了最后一个元素之外的所有元素
    def init[A](l: List[A]): List[A] = l match
    {
        case Nil => Nil
        case Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
    }

    // 练习3.10 写一个尾递归的foldLeft
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match
    {
        case Nil => z
        case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

    // 练习3.11 用foldLeft写一下sum
    def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

    // 练习3.11 用foldLeft写一下product
    def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

    // 练习3.11 用foldLeft写一下长度计算  3.9是foldRight再做一遍，就略了，思路一样
    def length[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

    // 练习3.12 用foldLeft写一下元素颠倒顺序的函数
    def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((t: List[A], a: A) => Cons(a, t))

    // 练习3.13 用foldLeft实现foldRight
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b: B, a: A) => f(a, b))

    // 练习3.13 用foldLeft实现foldRight, 答案还给了这个解法, 目前证明还可以做到, 用递推证明, 自己想不出来
    def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

    // 练习3.14 用foldLeft或foldRight实现append函数
    def appendUserFoldRight[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)(Cons(_, _))

    // 练习3.15 写一个函数将一组列表连接成一个单个列表
    def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)

    // 练习3.16 整数列表，所有元素加1
    def plusOne(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

    // 练习3.17 将List[Double]中的值转为String
    def double2string(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

    // 练习3.18 泛化的map函数
    def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

    // 练习3.19 filter
    def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

    // 练习3.20 flatMap
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldLeft(as, Nil: List[B])((t, h) => append(t, f(h)))

    // 练习3.21 用flatMap实现filter
    def filterUseFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(h => if (f(h)) List(h) else Nil)

    // 练习3.22 接收两个列表，通过对相应元素的相加构造出一个新的列表
    def listAdd(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match
    {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, listAdd(t1, t2))
    }

    // 练习3.23 对listAdd泛化
    def zipWith[A, B, C](l1 : List[A], l2:List[B], f : (A, B) => C) : List[C] = (l1, l2) match
    {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2, f))
    }
}

