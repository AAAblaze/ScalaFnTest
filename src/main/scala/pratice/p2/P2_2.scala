package pratice.p2

/**
  * 第20页练习2.2
  * 实现isSorted方法，检测Array[A]是否按照给定的比较函数排序：
  * def isSorted[A](as Array
  */
object P2_2
{
    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    {
        def loop(k: Int): Boolean =
        {
            if (k >= as.length - 1)
            {
                true
            }
            else if (!ordered(as(k), as(k + 1)))
            {
                false
            }
            else
            {
                loop(k + 1)
            }
        }

        loop(0)
    }

    def main(args: Array[String])
    {
        val ary = Array(0, 5, 13, 14, 15, 24)
        println(isSorted(ary, (x: Int, y: Int) => x < y))
    }

}

