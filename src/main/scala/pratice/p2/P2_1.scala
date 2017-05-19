package pratice.p2

/**
  * 第17页练习2.1
  * 写一个递归函数，来获取第n个斐波那契娄，前两个斐波那契数0和1，第n个数总是等于它前两个数的和
  * 序列开始为0、1、1、2、3、5、5
  * 应该定义为局部尾递归函数 def fib(n: Int): Int
  */
object P2_1
{
    def fib(n: Int): Int =
    {
        @annotation.tailrec
        def loop(n: Int, prev: Int, cur: Int): Int =
            if (n == 1) prev
            else loop(n - 1, cur, prev + cur)

        loop(n, 0, 1)
    }

    def main(args: Array[String])
    {
        println(fib(1))
    }
}
