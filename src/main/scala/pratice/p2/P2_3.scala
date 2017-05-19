package pratice.p2

/**
  * 第22页练习2.3
  * 我们看另一个柯里化的例子，把带有两个参数的函数f转换为只有一个参数的部分应用函数f
  * def curry[A, B, C](f: (A, B) => C): A => (B => C)
  */
object P2_3
{
    def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)
}
