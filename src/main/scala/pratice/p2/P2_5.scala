package pratice.p2

/**
  * 22页练习2.5
  * 实现一个高阶函数，可以组合两个函数为一个函数
  * def compose[A, B, C](f: B => C, g: A => B): A => C
  */
object P2_5
{
    def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}
